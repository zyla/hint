{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Interpreter.GHC
-- Copyright   :  (c) Daniel Gorin 2007
-- License     :  BSD-style
--
-- Maintainer  :  dgorin@dc.uba.ar
-- Stability   :  experimental
-- Portability :  non-portable (ghc api)
--
-- A Haskell interpreter built on top of the GHC API
-----------------------------------------------------------------------------
module Language.Haskell.Interpreter.GHC(
    InterpreterSession, newSession,
    --
    InterpreterError(..),
    --
    Interpreter, withSession,
    loadPrelude,
    --
    TypeChecked,
    typeChecks,
    typeCheck,
    typeOf,
    --
    compile,
    eval)

where

import qualified GHC
import qualified PackageConfig(stringToPackageId)

import qualified GHC.Exts(unsafeCoerce#)

import Language.Haskell.Syntax(HsQualType)

import Control.Monad.Trans(MonadIO(liftIO))
import Control.Monad.Reader(ReaderT, ask, runReaderT)
import Control.Monad.Error(Error(..), MonadError(..), ErrorT, runErrorT)

import Data.Typeable(Typeable)
import qualified Data.Typeable(typeOf)

import Language.Haskell.Interpreter.GHC.Conversions(GhcToHs(ghc2hs))

newtype Interpreter a = Interpreter{unInterpreter :: ReaderT InterpreterSession (ErrorT InterpreterError IO) a}

instance Monad Interpreter where
    return  = Interpreter . return
    i >>= f = Interpreter (unInterpreter i >>= unInterpreter . f)

instance Functor Interpreter where
    fmap f (Interpreter m) = Interpreter (fmap f m)

instance MonadIO Interpreter where
    liftIO = Interpreter . liftIO

instance MonadError InterpreterError Interpreter where
    throwError  = Interpreter . throwError
    catchError (Interpreter m) catchE = Interpreter (catchError m (\e -> unInterpreter $ catchE e))

-- | Executes the interpreter using a given session
withSession :: InterpreterSession -> Interpreter a -> IO (Either InterpreterError a)
withSession s i = runErrorT $ runReaderT (unInterpreter i) s


data InterpreterError = SomeError String
                      | TypeCheckingFailed String
                      deriving(Eq, Show)

instance Error InterpreterError where
    noMsg  = SomeError ""
    strMsg = SomeError

newtype InterpreterSession = InterpreterSession{theGhcSession :: GHC.Session}

-- | Builds new session, given the path to a GHC installation (e.g. \/opt\/local\/lib\/ghc-6.6)
newSession :: FilePath -> IO InterpreterSession
newSession ghcRoot =
    do
        ghcSession <- GHC.newSession GHC.Interactive $ Just ghcRoot
        --
        -- set HscTarget to HscInterpreted (must be done manually, default is HsAsm!)
        -- setSessionDynFlags loads info on packages availables. Call is mandatory!
        dflags <- GHC.getSessionDynFlags ghcSession
        GHC.setSessionDynFlags ghcSession dflags{GHC.hscTarget=GHC.HscInterpreted}
        --
        return $ InterpreterSession ghcSession

getGhcSession :: Interpreter GHC.Session
getGhcSession = fmap theGhcSession $ Interpreter ask

-- | By default, no module is loaded. Use this function to load the standard Prelude
loadPrelude :: Interpreter ()
loadPrelude =
    do
        ghcSession <- getGhcSession
        --
        let preludeModule = GHC.mkModule (PackageConfig.stringToPackageId "base")
                                         (GHC.mkModuleName "Prelude")
        liftIO $ GHC.setContext ghcSession [] [preludeModule]

-- | An expression that has been typechecked, along with its (phantom) type
newtype TypeChecked expr t = TypeChecked expr

instance Typeable a => Show (TypeChecked String a) where
    show (TypeChecked s) = concat ["TypeChecked (", s, ") :: ", show $ Data.Typeable.typeOf (undefined :: a)]

-- | Returns an abstract syntax tree of the type of the expression
typeOf :: String -> Interpreter HsQualType
typeOf expr =
    do
        ghcSession <- getGhcSession
        --
        maybe_ty <- liftIO $ GHC.exprType ghcSession expr
        case maybe_ty of
            Nothing -> throwError $ TypeCheckingFailed "No error message available... yet!"
            Just ty -> return $ ghc2hs (GHC.dropForAlls ty)

-- | Tests if the expression type checks
typeChecks :: String -> Interpreter Bool
typeChecks expr = (typeOf expr >> return True) `catchError` \_ -> return False

-- | Attempts to verify that expr has (monomorphic) type a
typeCheck :: Typeable a => String -> Interpreter (TypeChecked String a)
typeCheck expr = typeOf expr >> return (TypeChecked expr)

-- | Evaluates an expression whose (monomorphic) type has been typechecked to be a, into a value of type a.
-- Use with care: a must be monomorphic!
compile :: TypeChecked String a -> Interpreter a
compile (TypeChecked expr) =
    do
        ghcSession <- getGhcSession
        --
        maybe_expr_val <- liftIO $ GHC.compileExpr ghcSession expr
        case maybe_expr_val of
            Nothing       -> fail (unwords ["compilation of typechecked expression failed:", expr])
            Just expr_val -> return (GHC.Exts.unsafeCoerce# expr_val :: a)

-- | Evaluates a typechecked expression whose using Show (thus, it must be an instance of Show)
--
-- TODO Load package base, if necessary
eval :: Show a => TypeChecked String a -> Interpreter String
eval (TypeChecked expr) = compile =<< typeCheck show_expr
    where show_expr = unwords ["Prelude.show", "(", expr, ") "]

-- useful when debugging...
-- mySession = newSession "/opt/local/lib/ghc-6.6"

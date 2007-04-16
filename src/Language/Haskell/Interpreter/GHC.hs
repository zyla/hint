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
    typeOf,
    typeChecks,
    eval)

where

import qualified GHC
import qualified PackageConfig(stringToPackageId)

import qualified GHC.Exts(unsafeCoerce#)

import Language.Haskell.Syntax(HsQualType)

import Control.Monad(when)
import Control.Monad.Trans(MonadIO(liftIO))
import Control.Monad.Reader(ReaderT, ask, runReaderT)
import Control.Monad.Error(Error(..), MonadError(..), ErrorT, runErrorT)

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
                      | NoShowInstance HsQualType
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

-- | Evaluates the expression using Show (thus, it must be an instance of Show)
eval :: String -> Interpreter String
eval expr =
    do
        -- if this fails, expr doesn't compile. we just propagate the exception
        expr_type <- typeOf expr
        --
        let show_expr = unwords ["Prelude.show", "(", expr, ") "]
        hasShowInstance <- typeChecks show_expr
        when (not hasShowInstance) $
            throwError $ NoShowInstance expr_type
        --
        show_expr_val <- compileExpr show_expr
        return show_expr_val

compileExpr :: String -> Interpreter a
compileExpr expr =
    do
        ghcSession <- getGhcSession
        --
        maybe_expr_val <- liftIO $ GHC.compileExpr ghcSession expr
        case maybe_expr_val of
            Nothing       -> throwError $ TypeCheckingFailed (unwords ["compileExpr", expr])
            Just expr_val -> return (GHC.Exts.unsafeCoerce# expr_val :: a)

-- useful when debugging...
-- mySession = newSession "/opt/local/lib/ghc-6.6"

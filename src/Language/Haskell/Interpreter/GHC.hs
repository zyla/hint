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
    InterpreterError(..), GhcError,
    --
    Interpreter, withSession,
    loadPrelude,
    --
    typeChecks, typeOf,
    --
    interpret, as, infer,
    eval)

where

import qualified GHC
import qualified PackageConfig as GHC.P(stringToPackageId)
import qualified Outputable    as GHC.O(Outputable(ppr), PprStyle, withPprStyle, showSDoc)
import qualified SrcLoc        as GHC.S(SrcSpan)
import qualified ErrUtils      as GHC.E(Message)

import qualified GHC.Exts(unsafeCoerce#)

import Language.Haskell.Syntax(HsQualType)

import Control.Monad.Trans(MonadIO(liftIO))
import Control.Monad.Reader(ReaderT, ask, runReaderT)
import Control.Monad.Error(Error(..), MonadError(..), ErrorT, runErrorT)

import Control.Concurrent.MVar(MVar, newMVar, withMVar)
import Data.IORef(IORef, newIORef, modifyIORef, atomicModifyIORef)

import Data.Typeable(Typeable)
import qualified Data.Typeable(typeOf)

import Language.Haskell.Interpreter.GHC.Conversions(GhcToHs(ghc2hs))

newtype Interpreter a = Interpreter{unInterpreter :: ReaderT SessionState (ErrorT  InterpreterError IO) a}

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

-- | Executes the interpreter using a given session. This is a thread-safe operation,
-- if the InterpreterSession is in-use, the call will block until the other one finishes.
withSession :: InterpreterSession -> Interpreter a -> IO (Either InterpreterError a)
withSession s i = withMVar (sessionState s) $ \ss ->
    runErrorT . flip runReaderT ss $ unInterpreter i


data InterpreterError = SomeError String
                      | ParsingFailed [GhcError]
                      | TypeCheckingFailed [GhcError]
                      deriving(Eq, Show)

instance Error InterpreterError where
    noMsg  = SomeError ""
    strMsg = SomeError

-- I'm assuming operations on a ghcSession are not thread-safe. Besides, we need to
-- be sure that messages captured by the log handler correspond to a single operation.
-- Hence, we put the whole state on an MVar, and synchronize on it
newtype InterpreterSession = InterpreterSession {sessionState :: MVar SessionState}

data SessionState = SessionState{ghcSession     :: GHC.Session,
                                 ghcErrListRef  :: IORef [GhcError],
                                 modulesListRef :: IORef [FilePath]}

type GhcError = String

-- | Builds new session, given the path to a GHC installation (e.g. \/opt\/local\/lib\/ghc-6.6)
newSession :: FilePath -> IO InterpreterSession
newSession ghcRoot =
    do
        ghc_session      <- GHC.newSession GHC.Interactive $ Just ghcRoot
        --
        ghc_err_list_ref <- newIORef []
        mod_list_ref     <- newIORef []
        --
        let session_state = SessionState{ghcSession     =  ghc_session,
                                         ghcErrListRef  = ghc_err_list_ref,
                                         modulesListRef = mod_list_ref}
        --
        -- set HscTarget to HscInterpreted (must be done manually, default is HsAsm!)
        -- setSessionDynFlags loads info on packages availables. Call is mandatory!
        dflags <- GHC.getSessionDynFlags ghc_session
        let myFlags = dflags{GHC.hscTarget  = GHC.HscInterpreted,
                             GHC.log_action = mkLogHandler ghc_err_list_ref}
        GHC.setSessionDynFlags ghc_session myFlags
        --
        return . InterpreterSession =<< newMVar session_state

mkLogHandler :: IORef [GhcError] -> (GHC.Severity -> GHC.S.SrcSpan -> GHC.O.PprStyle -> GHC.E.Message -> IO ())
mkLogHandler r sev src style msg = modifyIORef r (errorEntry :)
    where errorEntry = unlines ["GHC ERROR:",
                                showSev sev,
                                GHC.O.showSDoc . GHC.O.withPprStyle style $ GHC.O.ppr src,
                                GHC.O.showSDoc . GHC.O.withPprStyle style $ msg]
          showSev GHC.SevInfo    = "SevInfo"
          showSev GHC.SevWarning = "SevWarning"
          showSev GHC.SevError   = "SevError"
          showSev GHC.SevFatal   = "SevFatal"

fromSessionState :: (SessionState -> a) -> Interpreter a
fromSessionState f = Interpreter $ fmap f ask

-- modifies the session state and returns the old value
modifySessionState :: Show a => (SessionState -> IORef a) -> (a -> a) -> Interpreter a
modifySessionState target f =
    do
        ref     <- fromSessionState target
        old_val <- liftIO $ atomicModifyIORef ref (\a -> (f a, a))
        return old_val

-- | By default, no module is loaded. Use this function to load the standard Prelude
loadPrelude :: Interpreter ()
loadPrelude =
    do
        ghc_session <- fromSessionState ghcSession
        --
        let prelude_module = GHC.mkModule (GHC.P.stringToPackageId "base")
                                          (GHC.mkModuleName "Prelude")
        liftIO $ GHC.setContext ghc_session [] [prelude_module]

-- | Loads the
--loadModulesFromSrc :: [FilePath] -> Interpreter ()
--loadModulesFromSrc fs =
--    do
--        ghcSession <- getGhcSession
--        --
        

-- | Returns an abstract syntax tree of the type of the expression
typeOf :: String -> Interpreter HsQualType
typeOf expr =
    do
        ghc_session <- fromSessionState ghcSession
        --
        ty <- mayFail $ GHC.exprType ghc_session expr
        --
        -- Unqualify necessary types (i.e., do not expose internals)
        unqual <- liftIO $ GHC.getPrintUnqual ghc_session
        return $ ghc2hs (GHC.dropForAlls ty, unqual)

-- | Tests if the expression type checks
typeChecks :: String -> Interpreter Bool
typeChecks expr = (typeOf expr >> return True) `catchError` \_ -> return False

-- | Convenience functions to be used with typeCheck to provide witnesses. Example:
--
--   interpret \"head [True,False]\" (as :: Bool)
--
--
as, infer :: Typeable a => a
as    = undefined
infer = undefined

-- | Evaluates an expression, given a witness for its monomorphic type.
interpret :: Typeable a => String -> a -> Interpreter a
interpret expr witness =
    do
        ghc_session <- fromSessionState ghcSession
        --
        let expr_typesig = concat ["(", expr, ") :: ", show $ Data.Typeable.typeOf witness]
        expr_val <- mayFail $ GHC.compileExpr ghc_session expr_typesig
        --
        return (GHC.Exts.unsafeCoerce# expr_val :: a)

-- | Evaluate show expr. Succeeds only if expr has type t and there is a Show instance for t
eval :: String -> Interpreter String
eval expr = interpret show_expr (as :: String)
    where show_expr = unwords ["Prelude.show", "(", expr, ") "]

mayFail :: IO (Maybe a) -> Interpreter a
mayFail ghc_action =
    do
        maybe_res <- liftIO ghc_action
        --
        e <- modifySessionState ghcErrListRef (const [])
        --
        case maybe_res of
            Nothing -> if null e
                           -- GHC is sending all parse errors straight to stderr,
                           -- so there is no easy way to collect them at the moment
                           then throwError $ ParsingFailed ["A parsing error must have occurred!"]
                           else throwError $ TypeCheckingFailed (reverse e)
            Just a  -> if null e
                           then return a
                           else fail "GHC reported errors and gave a result!"

-- useful when debugging...
mySession = newSession "/opt/local/lib/ghc-6.6"

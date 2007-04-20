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
    InterpreterSession, newSessionWith,
    --
    InterpreterError(..), GhcError(..), Phase(..),
    --
    Interpreter, withSession,
    loadPrelude,
    --
    typeChecks, typeOf, kindOf,
    --
    interpret, as, infer,
    eval)

where

import Prelude hiding(span)

import qualified GHC
import qualified PackageConfig as GHC.P(stringToPackageId)
import qualified Outputable    as GHC.O(PprStyle, withPprStyle, defaultErrStyle, showSDoc)
import qualified SrcLoc        as GHC.S(SrcSpan)
import qualified ErrUtils      as GHC.E(Message, mkLocMessage)

import qualified GHC.Exts(unsafeCoerce#)

import Control.Monad.Trans(MonadIO(liftIO))
import Control.Monad.Reader(ReaderT, ask, runReaderT)
import Control.Monad.Error(Error(..), MonadError(..), ErrorT, runErrorT)

import Control.Concurrent.MVar(MVar, newMVar, withMVar)
import Data.IORef(IORef, newIORef, modifyIORef, atomicModifyIORef)

import Data.Typeable(Typeable)
import qualified Data.Typeable(typeOf)

import Language.Haskell.Interpreter.GHC.Parsers(ParseResult(..), parseExpr, parseType)
import Language.Haskell.Interpreter.GHC.Conversions(FromGhcRep(..))

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


data InterpreterError = UnknownError String
                      | WontCompile [GhcError]
                      deriving Show

instance Error InterpreterError where
    noMsg  = UnknownError ""
    strMsg = UnknownError

-- I'm assuming operations on a ghcSession are not thread-safe. Besides, we need to
-- be sure that messages captured by the log handler correspond to a single operation.
-- Hence, we put the whole state on an MVar, and synchronize on it
newtype InterpreterSession = InterpreterSession {sessionState :: MVar SessionState}

data SessionState = SessionState{ghcSession     :: GHC.Session,
                                 ghcErrListRef  :: IORef [GhcError],
                                 ghcErrLogger   :: GhcErrLogger,
                                 modulesListRef :: IORef [FilePath]}

-- When intercepting errors reported by GHC, we only get a GHC.E.Message
-- and a GHC.S.SrcSpan. The latter holds the file name and the location
-- of the error. However, SrcSpan is abstract and it doesn't provide
-- functions to retrieve the line and column of the error... we can only
-- generate a string with this information. Maybe I can parse this string
-- later.... (sigh)
data GhcError = GhcError{phase  :: Phase, errMsg :: String} deriving Show

data Phase = ParsingPhase | TypeCheckingPhase deriving(Eq, Show)

mkGhcError :: Phase -> GHC.S.SrcSpan -> GHC.O.PprStyle -> GHC.E.Message -> GhcError
mkGhcError p span style msg = GhcError{phase  = p, errMsg = niceErrMsg}
    where niceErrMsg = GHC.O.showSDoc . GHC.O.withPprStyle style $ GHC.E.mkLocMessage span msg

type GhcErrLogger = GHC.Severity -> GHC.S.SrcSpan -> GHC.O.PprStyle -> GHC.E.Message -> IO ()

-- | Builds new session, given the path to a GHC installation (e.g. \/opt\/local\/lib\/ghc-6.6)
newSessionWith :: FilePath -> IO InterpreterSession
newSessionWith ghcRoot =
    do
        ghc_session      <- GHC.newSession GHC.Interactive $ Just ghcRoot
        --
        ghc_err_list_ref <- newIORef []
        mod_list_ref     <- newIORef []
        let log_handler  =  mkLogHandler ghc_err_list_ref
        --
        let session_state = SessionState{ghcSession     = ghc_session,
                                         ghcErrListRef  = ghc_err_list_ref,
                                         ghcErrLogger   = log_handler,
                                         modulesListRef = mod_list_ref}
        --
        -- set HscTarget to HscInterpreted (must be done manually, default is HsAsm!)
        -- setSessionDynFlags loads info on packages availables. Call is mandatory!
        -- also set a custom log handler, to intercept typechecking error messages :S
        dflags <- GHC.getSessionDynFlags ghc_session
        let myFlags = dflags{GHC.hscTarget  = GHC.HscInterpreted,
                             GHC.log_action = log_handler}
        GHC.setSessionDynFlags ghc_session myFlags
        --
        return . InterpreterSession =<< newMVar session_state

mkLogHandler :: IORef [GhcError] -> GhcErrLogger
mkLogHandler r _ src style msg = modifyIORef r (errorEntry :)
    where errorEntry = mkGhcError TypeCheckingPhase src style msg
--          showSev GHC.SevInfo    = "SevInfo"
--          showSev GHC.SevWarning = "SevWarning"
--          showSev GHC.SevError   = "SevError"
--          showSev GHC.SevFatal   = "SevFatal"

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


-- | Returns a string representation of the type of the expression
typeOf :: String -> Interpreter String
typeOf expr =
    do
        ghc_session <- fromSessionState ghcSession
        --
        -- First, make sure the expression has no syntax errors,
        -- for this is the only way we have to "intercept" this
        -- kind of errors
        failOnParseError parseExpr expr
        --
        ty <- mayFail $ GHC.exprType ghc_session expr
        --
        -- Unqualify necessary types (i.e., do not expose internals)
        unqual <- liftIO $ GHC.getPrintUnqual ghc_session
        return $ fromGhcRep (GHC.dropForAlls ty, unqual)

-- | Tests if the expression type checks
typeChecks :: String -> Interpreter Bool
typeChecks expr = (typeOf expr >> return True)
                  `catchError`
                  onCompilationError (\_ -> return False)

-- | Returns a string representation of the kind of the type expression
kindOf :: String -> Interpreter String
kindOf type_expr =
    do
        ghc_session <- fromSessionState ghcSession
        --
        -- First, make sure the expression has no syntax errors,
        -- for this is the only way we have to "intercept" this
        -- kind of errors
        failOnParseError parseType type_expr

        kind <- mayFail $ GHC.typeKind ghc_session type_expr
        --
        return $ fromGhcRep kind


-- | Convenience functions to be used with typeCheck to provide witnesses. Example:
--
--   interpret \"head [True,False]\" (as :: Bool)
--
--   interpret "head $ map show [True,False]" infer >>= flip interpret (as :: Bool)
as, infer :: Typeable a => a
as    = undefined
infer = undefined

-- | Evaluates an expression, given a witness for its monomorphic type.
interpret :: Typeable a => String -> a -> Interpreter a
interpret expr witness =
    do
        ghc_session <- fromSessionState ghcSession
        --
        -- First, make sure the expression has no syntax errors,
        -- for this is the only way we have to "intercept" this
        -- kind of errors
        failOnParseError parseExpr expr
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
        es <- modifySessionState ghcErrListRef (const [])
        --
        case maybe_res of
            Nothing -> if null es
                           then throwError $ UnknownError "Got no error message"
                           else throwError $ WontCompile (reverse es)
            Just a  -> if null es
                           then return a
                           else fail "GHC reported errors and also gave a result!"

failOnParseError :: (GHC.Session -> String -> IO ParseResult) -> String -> Interpreter ()
failOnParseError parser expr =
    do
        ghc_session <- fromSessionState ghcSession
        --
        parsed <- liftIO $ parser ghc_session expr
        --
        -- If there was a parsing error, do the "standard" error reporting
        res <- case parsed of
                   ParseOk             -> return (Just ())
                   --
                   ParseError span err ->
                       do
                       -- parsing failed, so we add a GhcError to the error list
                       -- (just like the log_action does on type checking errors)
                       let errorEntry = mkGhcError ParsingPhase span GHC.O.defaultErrStyle err
                       modifySessionState ghcErrListRef (errorEntry :)
                       --
                       -- behave like the rest of the GHC API functions do on error
                       return Nothing
        --
        -- "may Have Failed", actually :)
        mayFail (return res)

onCompilationError :: ([GhcError] -> Interpreter a) -> (InterpreterError -> Interpreter a)
onCompilationError recover = \interp_error -> case interp_error of
                                                  WontCompile errs -> recover errs
                                                  otherErr         -> throwError otherErr

-- useful when debugging...
mySession = newSessionWith "/opt/local/lib/ghc-6.6"

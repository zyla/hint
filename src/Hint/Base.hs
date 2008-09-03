module Hint.Base

where

import Prelude hiding ( span )

import Control.Monad.Reader
import Control.Monad.Error

import Control.Exception

import Data.IORef
import Control.Concurrent.MVar

import Data.Dynamic

import qualified GHC
import qualified GHC.Paths
import qualified Outputable as GHC.O
import qualified SrcLoc     as GHC.S
import qualified ErrUtils   as GHC.E


import qualified Hint.Compat as Compat
import Hint.Parsers

newtype Interpreter a =
    Interpreter{unInterpreter :: ReaderT SessionState
                                (ErrorT  InterpreterError
                                 IO)     a}
    deriving (Typeable, Functor, Monad, MonadIO)


instance MonadError InterpreterError Interpreter where
    throwError  = Interpreter . throwError
    catchError (Interpreter m) catchE = Interpreter $ m `catchError` (\e ->
                                                       unInterpreter $ catchE e)

data InterpreterError = UnknownError String
                      | WontCompile [GhcError]
                      | NotAllowed  String
                      -- | GhcExceptions from the underlying GHC API are caught
                      -- and rethrown as this.
                      | GhcException GHC.GhcException
                      deriving (Show, Typeable)

instance Error InterpreterError where
    noMsg  = UnknownError ""
    strMsg = UnknownError

data InterpreterState = St{all_mods_in_scope    :: Bool,
                           active_phantoms      :: [PhantomModule],
                           zombie_phantoms      :: [PhantomModule],
                           import_qual_hack_mod :: Maybe PhantomModule,
                           qual_imports         :: [(ModuleName, String)]}

initialState :: InterpreterState
initialState = St {all_mods_in_scope    = True,
                   active_phantoms      = [],
                   zombie_phantoms      = [],
                   import_qual_hack_mod = Nothing,
                   qual_imports         = []}

-- I'm assuming operations on a ghcSession are not thread-safe. Besides, we need
-- to be sure that messages captured by the log handler correspond to a single
-- operation. Hence, we put the whole state on an MVar, and synchronize on it
newtype InterpreterSession =
    InterpreterSession {sessionState :: MVar SessionState}

data SessionState = SessionState{internalState   :: IORef InterpreterState,
                                 --
                                 ghcSession      :: GHC.Session,
                                 ghcErrListRef   :: IORef [GhcError],
                                 ghcErrLogger    :: GhcErrLogger}

-- When intercepting errors reported by GHC, we only get a GHC.E.Message
-- and a GHC.S.SrcSpan. The latter holds the file name and the location
-- of the error. However, SrcSpan is abstract and it doesn't provide
-- functions to retrieve the line and column of the error... we can only
-- generate a string with this information. Maybe I can parse this string
-- later.... (sigh)
data GhcError = GhcError{errMsg :: String} deriving Show

mkGhcError :: GHC.S.SrcSpan -> GHC.O.PprStyle -> GHC.E.Message -> GhcError
mkGhcError src_span style msg = GhcError{errMsg = niceErrMsg}
    where niceErrMsg = GHC.O.showSDoc . GHC.O.withPprStyle style $
                         GHC.E.mkLocMessage src_span msg

mapGhcExceptions :: (String -> InterpreterError) -> IO a -> Interpreter a
mapGhcExceptions buildEx action =
    do  r <- liftIO $ tryJust ghcExceptions action
        either (throwError . buildEx . flip GHC.showGhcException []) return r

ghcExceptions :: Exception -> Maybe GHC.GhcException
ghcExceptions (DynException a) = fromDynamic a
ghcExceptions  _               = Nothing


type GhcErrLogger = GHC.Severity
                 -> GHC.S.SrcSpan
                 -> GHC.O.PprStyle
                 -> GHC.E.Message
                 -> IO ()

-- | Module names are _not_ filepaths.
type ModuleName = String

-- ================= Creating a session =========================

-- | Builds a new session using the (hopefully) correct path to the GHC in use.
-- (the path is determined at build time of the package)
newSession :: IO InterpreterSession
newSession = newSessionUsing GHC.Paths.libdir

-- | Builds a new session, given the path to a GHC installation
--  (e.g. \/usr\/local\/lib\/ghc-6.6).
newSessionUsing :: FilePath -> IO InterpreterSession
newSessionUsing ghc_root =
    do
        ghc_session      <- Compat.newSession ghc_root
        --
        initial_state    <- newIORef initialState
        ghc_err_list_ref <- newIORef []
        let log_handler  =  mkLogHandler ghc_err_list_ref
        --
        let session_state = SessionState{internalState   = initial_state,
                                         --
                                         ghcSession      = ghc_session,
                                         ghcErrListRef   = ghc_err_list_ref,
                                         ghcErrLogger    = log_handler}
        --
        -- Set a custom log handler, to intercept error messages :S
        -- Observe that, setSessionDynFlags loads info on packages available;
        -- calling this function once is mandatory! (nevertheless it was most
        -- likely already done in Compat.newSession...)
        dflags <- GHC.getSessionDynFlags ghc_session
        GHC.setSessionDynFlags ghc_session dflags{GHC.log_action = log_handler}
        --
        InterpreterSession `liftM` newMVar session_state

mkLogHandler :: IORef [GhcError] -> GhcErrLogger
mkLogHandler r _ src style msg = modifyIORef r (errorEntry :)
    where errorEntry = mkGhcError src style msg


-- ================= Executing the interpreter ==================

-- | Executes the interpreter using a given session. This is a thread-safe
--   operation, if the InterpreterSession is in-use, the call will block until
--   the other one finishes.
--
--   In case of error, it will throw a dynamic InterpreterError exception.
withSession :: InterpreterSession -> Interpreter a -> IO a
withSession s i = withMVar (sessionState s) $ \ss ->
    do err_or_res <- runErrorT . flip runReaderT ss $ unInterpreter i
       either throwDyn return err_or_res
    `catchDyn` rethrowGhcException

rethrowGhcException :: GHC.GhcException -> IO a
rethrowGhcException = throwDyn . GhcException



-- ================ Handling the interpreter state =================

fromSessionState :: (SessionState -> a) -> Interpreter a
fromSessionState f = Interpreter $ fmap f ask

-- modifies a ref in the session state and returns the old value
modifySessionStateRef :: (SessionState -> IORef a) -> (a -> a) -> Interpreter a
modifySessionStateRef target f =
    do ref     <- fromSessionState target
       old_val <- liftIO $ atomicModifyIORef ref (\a -> (f a, a))
       return old_val

fromState :: (InterpreterState -> a) -> Interpreter a
fromState f = do ref_st <- fromSessionState internalState
                 liftIO $ f `fmap` readIORef ref_st

onState :: (InterpreterState -> InterpreterState) -> Interpreter ()
onState f = modifySessionStateRef internalState f >> return ()

-- =============== Error handling ==============================

mayFail :: IO (Maybe a) -> Interpreter a
mayFail ghc_action =
    do
        maybe_res <- liftIO ghc_action
        --
        es <- modifySessionStateRef ghcErrListRef (const [])
        --
        case maybe_res of
            Nothing -> if null es
                         then throwError $ UnknownError "Got no error message"
                         else throwError $ WontCompile (reverse es)
            Just a  -> if null es
                         then return a
                         else fail "GHC reported errors and also gave a result!"

finally :: Interpreter a -> Interpreter () -> Interpreter a
finally action clean_up = do r <- protected_action
                             clean_up
                             return r
    where protected_action = action
                             `catchError`
                             (\e -> do clean_up `catchError` (\_ -> return ())
                                       throwError e)

-- ================ Misc ===================================

-- this type ought to go in Hint.Context, but ghc dislikes cyclic imports...
data PhantomModule = PhantomModule{pm_name :: ModuleName, pm_file :: FilePath}
                   deriving (Eq, Show)

findModule :: ModuleName -> Interpreter GHC.Module
findModule mn =
    do
        ghc_session <- fromSessionState ghcSession
        --
        let mod_name = GHC.mkModuleName mn
        mapGhcExceptions NotAllowed $ GHC.findModule ghc_session
                                                     mod_name
                                                     Nothing

failOnParseError :: (GHC.Session -> String -> IO ParseResult)
                 -> String
                 -> Interpreter ()
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
                           -- parsing failed, so we report it just as all
                           -- other errors get reported....
                           logger <- fromSessionState ghcErrLogger
                           liftIO $ logger GHC.SevError
                                           span
                                           GHC.O.defaultErrStyle
                                           err
                           --
                           -- behave like the rest of the GHC API functions
                           -- do on error...
                           return Nothing
        --
        -- "may Have Already Failed", actually :)
        mayFail (return res)

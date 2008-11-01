module Hint.Base

where

import Prelude hiding ( span )

import Control.Monad.Reader
import Control.Monad.Error

import Control.Exception

import Data.IORef

import Data.Dynamic

import qualified Hint.GHC as GHC
import qualified GHC.Paths

import qualified Hint.Compat as Compat

-- this requires FlexibleContexts
class (MonadIO m, MonadError InterpreterError m) => MonadInterpreter m where
    fromSession      :: (InterpreterSession -> a) -> m a
    modifySessionRef :: (InterpreterSession -> IORef a) -> (a -> a) -> m a

newtype InterpreterT m a = InterpreterT{
                             unInterpreterT :: ReaderT InterpreterSession
                                               (ErrorT InterpreterError m) a}
    deriving (Functor, Monad, MonadIO)

type Interpreter = InterpreterT IO

instance MonadIO m => MonadInterpreter (InterpreterT m) where
    fromSession f = InterpreterT $ fmap f ask
    --
    modifySessionRef target f =
        do ref     <- fromSession target
           old_val <- liftIO $ atomicModifyIORef ref (\a -> (f a, a))
           return old_val

instance MonadTrans InterpreterT where
    lift = InterpreterT . lift . lift

instance Monad m => MonadError InterpreterError (InterpreterT m) where
    throwError  = InterpreterT . throwError
    catchError (InterpreterT m) catchE = InterpreterT $
                                             m `catchError`
                                               (\e -> unInterpreterT $ catchE e)

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

data InterpreterSession = InterpreterSession {
                              internalState   :: IORef InterpreterState,
                              --
                              ghcSession      :: GHC.Session,
                              ghcErrListRef   :: IORef [GhcError],
                              ghcErrLogger    :: GhcErrLogger}

-- When intercepting errors reported by GHC, we only get a ErrUtils.Message
-- and a SrcLoc.SrcSpan. The latter holds the file name and the location
-- of the error. However, SrcSpan is abstract and it doesn't provide
-- functions to retrieve the line and column of the error... we can only
-- generate a string with this information. Maybe I can parse this string
-- later.... (sigh)
data GhcError = GhcError{errMsg :: String} deriving Show

mkGhcError :: GHC.SrcSpan -> GHC.PprStyle -> GHC.Message -> GhcError
mkGhcError src_span style msg = GhcError{errMsg = niceErrMsg}
    where niceErrMsg = GHC.showSDoc . GHC.withPprStyle style $
                         GHC.mkLocMessage src_span msg

mapGhcExceptions :: MonadInterpreter m
                 => (String -> InterpreterError)
                 -> m a
                 -> m a
mapGhcExceptions buildEx action =
    do  action
          `catchError` (\err -> case err of
                                    GhcException e -> throwError (remap e)
                                    _              -> throwError err)
    where remap =  buildEx . flip GHC.showGhcException []

ghcExceptions :: Exception -> Maybe GHC.GhcException
ghcExceptions (DynException a) = fromDynamic a
ghcExceptions  _               = Nothing


type GhcErrLogger = GHC.Severity
                 -> GHC.SrcSpan
                 -> GHC.PprStyle
                 -> GHC.Message
                 -> IO ()

-- | Module names are _not_ filepaths.
type ModuleName = String


class GhcApiAdjuster (m :: * -> *) a b | m a -> b, a b -> m where
    runGhc  :: (GHC.Session -> a) -> b

instance MonadInterpreter m => GhcApiAdjuster m (IO a) (m a) where
    runGhc f = do s <- fromSession ghcSession
                  r <- liftIO $ f' s
                  either throwError return r
        where f' = tryJust (fmap GhcException . ghcExceptions) . f

-- this instance requires UndecidableInstances
instance (MonadInterpreter m, GhcApiAdjuster m b b')
      => GhcApiAdjuster m (a -> b) (a -> b') where
    runGhc f = \a -> runGhc (flip f a)


-- ================= Creating a session =========================

-- | Builds a new session using ghc-paths to find the path to the installation
newSession :: IO InterpreterSession
newSession =
    do
        ghc_session      <- Compat.newSession GHC.Paths.libdir
        --
        initial_state    <- newIORef initialState
        ghc_err_list_ref <- newIORef []
        let log_handler  =  mkLogHandler ghc_err_list_ref
        --
        -- Set a custom log handler, to intercept error messages :S
        -- Observe that, setSessionDynFlags loads info on packages available;
        -- calling this function once is mandatory! (nevertheless it was most
        -- likely already done in Compat.newSession...)
        dflags <- GHC.getSessionDynFlags ghc_session
        GHC.setSessionDynFlags ghc_session dflags{GHC.log_action = log_handler}
        --
        return InterpreterSession{internalState   = initial_state,
                                  --
                                  ghcSession      = ghc_session,
                                  ghcErrListRef   = ghc_err_list_ref,
                                  ghcErrLogger    = log_handler}

mkLogHandler :: IORef [GhcError] -> GhcErrLogger
mkLogHandler r _ src style msg = modifyIORef r (errorEntry :)
    where errorEntry = mkGhcError src style msg


-- ================= Executing the interpreter ==================

-- | Executes the interpreter.
--
--   In case of error, it will throw a dynamic InterpreterError exception.
runInterpreter :: MonadIO m => InterpreterT m a -> m a
runInterpreter interpreter =
    do s <- liftIO (newSession `catchDyn` rethrowGhcException)
       err_or_res <- runErrorT . flip runReaderT s $ unInterpreterT interpreter
       either (liftIO . throwDyn) return err_or_res

rethrowGhcException :: GHC.GhcException -> IO a
rethrowGhcException = throwDyn . GhcException



-- ================ Handling the interpreter state =================

fromState :: MonadInterpreter m => (InterpreterState -> a) -> m a
fromState f = do ref_st <- fromSession internalState
                 liftIO $ f `fmap` readIORef ref_st

onState :: MonadInterpreter m => (InterpreterState -> InterpreterState) -> m ()
onState f = modifySessionRef internalState f >> return ()

-- =============== Error handling ==============================

mayFail :: MonadInterpreter m => m (Maybe a) -> m a
mayFail action =
    do
        maybe_res <- action
        --
        es <- modifySessionRef ghcErrListRef (const [])
        --
        case (maybe_res, null es) of
            (Nothing,True)  -> throwError $ UnknownError "Got no error message"
            (Nothing,False) -> throwError $ WontCompile (reverse es)
            (Just a, True)  -> return a
            (Just _, False) -> fail "GHC reported errors but returned a result!"

finally :: MonadInterpreter m => m a -> m () -> m a
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

findModule :: MonadInterpreter m => ModuleName -> m GHC.Module
findModule mn = mapGhcExceptions NotAllowed $
                    runGhc GHC.findModule mod_name Nothing
    where mod_name = GHC.mkModuleName mn


moduleIsLoaded :: MonadInterpreter m => ModuleName -> m Bool
moduleIsLoaded mn = (findModule mn >> return True)
                   `catchError` (\e -> case e of
                                        NotAllowed{} -> return False
                                        _            -> throwError e)


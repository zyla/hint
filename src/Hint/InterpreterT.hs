module Hint.InterpreterT (
    InterpreterT, Interpreter, runInterpreter,
)

where

import Prelude hiding ( catch )

import Hint.Base

import Control.Monad.Reader
import Control.Monad.Error

import Data.IORef
#if __GLASGOW_HASKELL__ < 610
import Data.Dynamic
#endif

import qualified GHC.Paths

import qualified Hint.GHC as GHC
import qualified Hint.Compat as Compat
import Hint.Compat.Exceptions

type Interpreter = InterpreterT IO

#if __GLASGOW_HASKELL__ < 610

newtype InterpreterT m a = InterpreterT{
                             unInterpreterT :: ReaderT InterpreterSession
                                               (ErrorT InterpreterError m) a}
    deriving (Functor, Monad, MonadIO, MonadCatchIO)

execute :: (MonadCatchIO m, Functor m)
        => InterpreterSession
        -> InterpreterT m a
        -> m (Either InterpreterError a)
execute s = runErrorT . flip runReaderT s . unInterpreterT

instance MonadTrans InterpreterT where
    lift = InterpreterT . lift . lift

runGhc_impl :: (MonadCatchIO m, Functor m) => RunGhc (InterpreterT m) a
runGhc_impl f = do s <- fromSession versionSpecific -- i.e. the ghc session
                   r <- liftIO $ f' s
                   either throwError return r
    where f' = tryJust (fmap (GhcException . showGhcEx) . ghcExceptions) . f
          ghcExceptions (DynException e) = fromDynamic e
          ghcExceptions  _               = Nothing

#else -- ghc >= 6.10

newtype InterpreterT m a = InterpreterT{
                             unInterpreterT :: ReaderT  InterpreterSession
                                              (ErrorT   InterpreterError
                                              (GHC.GhcT m)) a}
    deriving (Functor, Monad, MonadIO, MonadCatchIO)

execute :: (MonadCatchIO m, Functor m)
        => InterpreterSession
        -> InterpreterT m a
        -> m (Either InterpreterError a)
execute s = GHC.runGhcT (Just GHC.Paths.libdir)
          . runErrorT
          . flip runReaderT s
          . unInterpreterT

instance MonadTrans InterpreterT where
    lift = InterpreterT . lift . lift . lift

runGhc_impl :: (MonadCatchIO m, Functor m) => RunGhc (InterpreterT m) a
runGhc_impl a = InterpreterT (lift (lift a))
                `catches`
                 [Handler (\(e :: GHC.SourceError)  -> rethrowWC e),
                  Handler (\(e :: GHC.GhcApiError)  -> rethrowGE $ show e),
                  Handler (\(e :: GHC.GhcException) -> rethrowGE $ showGhcEx e)]
    where rethrowGE = throwError . GhcException
          rethrowWC = throwError
                    . WontCompile
                    . map (GhcError . show)
                    . GHC.bagToList
                    . GHC.srcErrorMessages
#endif

showGhcEx :: GHC.GhcException -> String
showGhcEx = flip GHC.showGhcException ""

-- ================= Executing the interpreter ==================

initialize :: (MonadCatchIO m, Functor m) => InterpreterT m ()
initialize =
    do log_handler <- fromSession ghcErrLogger
       --
       -- Set a custom log handler, to intercept error messages :S
       -- Observe that, setSessionDynFlags loads info on packages
       -- available; calling this function once is mandatory!
       dflags <- runGhc GHC.getSessionDynFlags
       let dflags' = Compat.configureDynFlags dflags
       runGhc1 GHC.setSessionDynFlags dflags'{GHC.log_action = log_handler}
       return ()

-- | Executes the interpreter. Returns @Left InterpreterError@ in case of error.
--
runInterpreter :: (MonadCatchIO m, Functor m)
               => InterpreterT m a
               -> m (Either InterpreterError a)
runInterpreter action =
    do s <- newInterpreterSession `catch` rethrowGhcException
       execute s (initialize >> action)
    where rethrowGhcException   = throw . GhcException . showGhcEx
#if __GLASGOW_HASKELL__ < 610
          newInterpreterSession =  do s <- liftIO $
                                             Compat.newSession GHC.Paths.libdir
                                      newSessionData s
#else -- GHC >= 610
          newInterpreterSession = newSessionData ()
#endif

initialState :: InterpreterState
initialState = St {all_mods_in_scope    = True,
                   active_phantoms      = [],
                   zombie_phantoms      = [],
                   import_qual_hack_mod = Nothing,
                   qual_imports         = []}


newSessionData :: MonadIO m => a -> m (SessionData a)
newSessionData  a = do initial_state    <- liftIO $ newIORef initialState
                       ghc_err_list_ref <- liftIO $ newIORef []
                       return SessionData{
                                internalState   = initial_state,
                                versionSpecific = a,
                                ghcErrListRef   = ghc_err_list_ref,
                                ghcErrLogger    = mkLogHandler ghc_err_list_ref
                              }

mkLogHandler :: IORef [GhcError] -> GhcErrLogger
mkLogHandler r _ src style msg = modifyIORef r (errorEntry :)
    where errorEntry = mkGhcError src style msg

mkGhcError :: GHC.SrcSpan -> GHC.PprStyle -> GHC.Message -> GhcError
mkGhcError src_span style msg = GhcError{errMsg = niceErrMsg}
    where niceErrMsg = GHC.showSDoc . GHC.withPprStyle style $
                         GHC.mkLocMessage src_span msg


-- The MonadInterpreter instance

instance (MonadCatchIO m, Functor m) => MonadInterpreter (InterpreterT m) where
    fromSession f = InterpreterT $ fmap f ask
    --
    modifySessionRef target f =
        do ref     <- fromSession target
           old_val <- liftIO $ atomicModifyIORef ref (\a -> (f a, a))
           return old_val
    --
    runGhc a = runGhc_impl a

instance Monad m => MonadError InterpreterError (InterpreterT m) where
    throwError  = InterpreterT . throwError
    catchError (InterpreterT m) catchE = InterpreterT $
                                             m `catchError`
                                              (\e -> unInterpreterT $ catchE e)

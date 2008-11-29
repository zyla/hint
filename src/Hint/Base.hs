module Hint.Base (
    MonadInterpreter(..),
    InterpreterT, Interpreter, runInterpreter,
    --
    GhcError(..), InterpreterError(..), finally, mayFail,
    --
    InterpreterSession, SessionData(..),
    InterpreterState(..), fromState, onState,
    --
    runGhc1, runGhc2, runGhc3, runGhc4, runGhc5,
    --
    ModuleName, PhantomModule(..),
    findModule, moduleIsLoaded,
)

where

import Prelude hiding ( span, catch )

import Control.Monad.Reader
import Control.Monad.Error

import Data.IORef

import Data.Dynamic

import qualified Hint.GHC as GHC
import qualified GHC.Paths

import Hint.Compat.Exceptions
import qualified Hint.Compat as Compat

-- this requires FlexibleContexts
class (MonadCatchIO m,MonadError InterpreterError m) => MonadInterpreter m where
    fromSession      :: FromSession m a
    modifySessionRef :: ModifySessionRef m a
    runGhc           :: RunGhc m a

-- this is for hiding the actual types in haddock
type FromSession      m a = (InterpreterSession -> a) -> m a
type ModifySessionRef m a = (InterpreterSession -> IORef a) -> (a -> a) -> m a

type Interpreter = InterpreterT IO

#if __GLASGOW_HASKELL__ < 610
type InterpreterSession = SessionData GHC.Session

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

type RunGhc  m a           = (GHC.Session -> IO a)
                          -> m a
type RunGhc1 m a b         = (GHC.Session -> a -> IO b)
                          -> (a -> m b)
type RunGhc2 m a b c       = (GHC.Session -> a -> b -> IO c)
                          -> (a -> b -> m c)
type RunGhc3 m a b c d     = (GHC.Session -> a -> b -> c -> IO d)
                          -> (a -> b -> c -> m d)

type RunGhc4 m a b c d e   = (GHC.Session -> a -> b -> c -> d -> IO e)
                          -> (a -> b -> c -> d -> m e)

type RunGhc5 m a b c d e f = (GHC.Session -> a -> b -> c -> d -> e -> IO f)
                          -> (a -> b -> c -> d -> e -> m f)

adjust :: (a -> b -> c) -> (b -> a -> c)
adjust f = flip f

runGhc_impl :: (MonadCatchIO m, Functor m) => RunGhc (InterpreterT m) a
runGhc_impl f = do s <- fromSession versionSpecific -- i.e. the ghc session
                   r <- liftIO $ f' s
                   either throwError return r
    where f' = tryJust (fmap GhcException . ghcExceptions) . f
          ghcExceptions (DynException e) = fromDynamic e
          ghcExceptions  _               = Nothing

#else -- ghc >= 6.10
type InterpreterSession = SessionData ()

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

type RunGhc  m a =
    (forall n.(MonadCatchIO n,Functor n) => GHC.GhcT n a)
 -> m a

type RunGhc1 m a b =
    (forall n.(MonadCatchIO n, Functor n) => a -> GHC.GhcT n b)
 -> (a -> m b)

type RunGhc2 m a b c =
    (forall n.(MonadCatchIO n, Functor n) => a -> b -> GHC.GhcT n c)
 -> (a -> b -> m c)

type RunGhc3 m a b c d =
    (forall n.(MonadCatchIO n, Functor n) => a -> b -> c -> GHC.GhcT n d)
 -> (a -> b -> c -> m d)

type RunGhc4 m a b c d e =
    (forall n.(MonadCatchIO n, Functor n) => a -> b -> c -> d -> GHC.GhcT n e)
 -> (a -> b -> c -> d -> m e)

type RunGhc5 m a b c d e f =
    (forall n.(MonadCatchIO n, Functor n) => a->b->c->d->e->GHC.GhcT n f)
 -> (a -> b -> c -> d -> e -> m f)

adjust :: (a -> b) -> (a -> b)
adjust = id

runGhc_impl :: (MonadCatchIO m, Functor m) => RunGhc (InterpreterT m) a
runGhc_impl a = do r <- InterpreterT (lift (lift a'))
                   either throwError return r
    where a' = tryJust (Just . GhcException) a

instance Exception InterpreterError

#endif


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

data SessionData a = SessionData {
                       internalState   :: IORef InterpreterState,
                       versionSpecific :: a,
                       ghcErrListRef   :: IORef [GhcError],
                       ghcErrLogger    :: GhcErrLogger
                     }

newSessionData :: MonadIO m => a -> m (SessionData a)
newSessionData  a = do initial_state    <- liftIO $ newIORef initialState
                       ghc_err_list_ref <- liftIO $ newIORef []
                       return SessionData{
                                internalState   = initial_state,
                                versionSpecific = a,
                                ghcErrListRef   = ghc_err_list_ref,
                                ghcErrLogger    = mkLogHandler ghc_err_list_ref
                              }


-- When intercepting errors reported by GHC, we only get a ErrUtils.Message
-- and a SrcLoc.SrcSpan. The latter holds the file name and the location
-- of the error. However, SrcSpan is abstract and it doesn't provide
-- functions to retrieve the line and column of the error... we can only
-- generate a string with this information. Maybe I can parse this string
-- later.... (sigh)
newtype GhcError = GhcError{errMsg :: String} deriving Show

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


type GhcErrLogger = GHC.Severity
                 -> GHC.SrcSpan
                 -> GHC.PprStyle
                 -> GHC.Message
                 -> IO ()

-- | Module names are _not_ filepaths.
type ModuleName = String

runGhc1 :: MonadInterpreter m => RunGhc1 m a b
runGhc1 f a = runGhc (adjust f a)

runGhc2 :: MonadInterpreter m => RunGhc2 m a b c
runGhc2 f a = runGhc1 (adjust f a)

runGhc3 :: MonadInterpreter m => RunGhc3 m a b c d
runGhc3 f a = runGhc2 (adjust f a)

runGhc4 :: MonadInterpreter m => RunGhc4 m a b c d e
runGhc4 f a = runGhc3 (adjust f a)

runGhc5 :: MonadInterpreter m => RunGhc5 m a b c d e f
runGhc5 f a = runGhc4 (adjust f a)

-- ================= Executing the interpreter ==================

mkLogHandler :: IORef [GhcError] -> GhcErrLogger
mkLogHandler r _ src style msg = modifyIORef r (errorEntry :)
    where errorEntry = mkGhcError src style msg

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
    where rethrowGhcException   = throw . GhcException
#if __GLASGOW_HASKELL__ < 610
          newInterpreterSession =  do s <- liftIO $
                                             Compat.newSession GHC.Paths.libdir
                                      newSessionData s
#else -- GHC >= 610
          newInterpreterSession = newSessionData ()
#endif

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
            (Just _, False) -> fail $ "GHC returned a result but said: " ++
                                      show es

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
                    runGhc2 GHC.findModule mod_name Nothing
    where mod_name = GHC.mkModuleName mn


moduleIsLoaded :: MonadInterpreter m => ModuleName -> m Bool
moduleIsLoaded mn = (findModule mn >> return True)
                   `catchError` (\e -> case e of
                                        NotAllowed{} -> return False
                                        _            -> throwError e)

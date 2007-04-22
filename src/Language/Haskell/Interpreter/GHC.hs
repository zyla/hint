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
    InterpreterError(..), GhcError(..),
    --
    Interpreter, withSession,
    --
    ModuleName, loadPrelude,
    loadModules, getLoadedModules, setTopLevelModules,
    setImports,
    reset,
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

import Control.Monad(liftM, filterM, guard, when)
import Control.Monad.Trans(MonadIO(liftIO))
import Control.Monad.Reader(ReaderT, ask, runReaderT)
import Control.Monad.Error(Error(..), MonadError(..), ErrorT, runErrorT)

import Control.Concurrent.MVar(MVar, newMVar, withMVar)
import Data.IORef(IORef, newIORef, modifyIORef, atomicModifyIORef)

import Control.Exception(Exception(DynException), tryJust)

import Data.Typeable(Typeable)
import qualified Data.Typeable(typeOf)
import Data.Dynamic(fromDynamic)

import Data.List((\\))

import Language.Haskell.Interpreter.GHC.Parsers(ParseResult(..), parseExpr, parseType)
import Language.Haskell.Interpreter.GHC.Conversions(FromGhcRep(..))

newtype Interpreter a = Interpreter{unInterpreter :: ReaderT SessionState (ErrorT  InterpreterError IO) a}
                        deriving(Typeable)

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
                      | NotAllowed  String
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
                                 ghcErrLogger   :: GhcErrLogger}

-- When intercepting errors reported by GHC, we only get a GHC.E.Message
-- and a GHC.S.SrcSpan. The latter holds the file name and the location
-- of the error. However, SrcSpan is abstract and it doesn't provide
-- functions to retrieve the line and column of the error... we can only
-- generate a string with this information. Maybe I can parse this string
-- later.... (sigh)
data GhcError = GhcError{errMsg :: String} deriving Show

mkGhcError :: GHC.S.SrcSpan -> GHC.O.PprStyle -> GHC.E.Message -> GhcError
mkGhcError span style msg = GhcError{errMsg = niceErrMsg}
    where niceErrMsg = GHC.O.showSDoc . GHC.O.withPprStyle style $ GHC.E.mkLocMessage span msg

type GhcErrLogger = GHC.Severity -> GHC.S.SrcSpan -> GHC.O.PprStyle -> GHC.E.Message -> IO ()

-- | Builds new session, given the path to a GHC installation (e.g. \/opt\/local\/lib\/ghc-6.6)
newSessionWith :: FilePath -> IO InterpreterSession
newSessionWith ghcRoot =
    do
        ghc_session      <- GHC.newSession GHC.Interactive $ Just ghcRoot
        --
        ghc_err_list_ref <- newIORef []
        let log_handler  =  mkLogHandler ghc_err_list_ref
        --
        let session_state = SessionState{ghcSession     = ghc_session,
                                         ghcErrListRef  = ghc_err_list_ref,
                                         ghcErrLogger   = log_handler}
        --
        -- set HscTarget to HscInterpreted (must be done manually, default is HsAsm!)
        -- setSessionDynFlags loads info on packages availables. Call is mandatory!
        -- also set a custom log handler, to intercept error messages :S
        dflags <- GHC.getSessionDynFlags ghc_session
        let myFlags = dflags{GHC.hscTarget  = GHC.HscInterpreted,
                             GHC.log_action = log_handler}
        GHC.setSessionDynFlags ghc_session myFlags
        --
        return . InterpreterSession =<< newMVar session_state

mkLogHandler :: IORef [GhcError] -> GhcErrLogger
mkLogHandler r _ src style msg = modifyIORef r (errorEntry :)
    where errorEntry = mkGhcError src style msg
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

-- | Module names are _not_ filepaths
type ModuleName = String

-- | Tries to load all the requested modules from their source file.
--   Modules my be indicated by their ModuleName (e.g. \"My.Module\") or
--   by the full path to its source file.
--
-- The interpreter is "reset" both before loading the modules and in the event
-- of an error.
loadModules :: [String] -> Interpreter ()
loadModules fs =
    do
        ghc_session <- fromSessionState ghcSession
        --
        -- first, unload everything
        reset
        --
        let doLoad = mayFail $ do
            targets <- mapM (\f -> GHC.guessTarget f Nothing) fs
            --
            GHC.setTargets ghc_session targets
            res <- GHC.load ghc_session GHC.LoadAllTargets
            return $ guard (isSucceeded res) >> Just ()
        --
        doLoad `catchError` (\e -> reset >> throwError e)
        --
        return ()

-- | Returns the list of modules loaded with "loadModules"
getLoadedModules :: Interpreter [ModuleName]
getLoadedModules = liftM (map modNameFromSummary) getLoadedModSummaries

modNameFromSummary :: GHC.ModSummary -> ModuleName
modNameFromSummary =  modNameFromModule . GHC.ms_mod

modNameFromModule :: GHC.Module -> ModuleName
modNameFromModule = GHC.moduleNameString . GHC.moduleName

getLoadedModSummaries :: Interpreter [GHC.ModSummary]
getLoadedModSummaries =
    do
        ghc_session <- fromSessionState ghcSession
        --
        all_mod_summ <- liftIO $ GHC.getModuleGraph ghc_session
        filterM (liftIO . GHC.isLoaded ghc_session . GHC.ms_mod_name) all_mod_summ

-- | Sets the modules whose context is used during evaluation. All bindings
--   of these modules are in scope, not only those exported.
--
--   Modules must be interpreted to use this function
setTopLevelModules :: [ModuleName] -> Interpreter ()
setTopLevelModules ms =
    do
        ghc_session <- fromSessionState ghcSession
        --
        loaded_mods_ghc <- getLoadedModSummaries
        --
        let not_loaded = ms \\ map modNameFromSummary loaded_mods_ghc
        when (not . null $ not_loaded) $
            throwError $ NotAllowed ("These modules have not been previously loaded:\n" ++ unlines not_loaded)
        --
        ms_mods <- mapM findModule ms
        --
        not_interpreted <- liftIO $ filterM (liftM not . GHC.moduleIsInterpreted ghc_session) ms_mods
        when (not . null $ not_interpreted) $
            throwError $ NotAllowed ("These modules are not interpreted:\n" ++ unlines (map modNameFromModule not_interpreted))
        --
        liftIO $ do
            (_, old_imports) <- GHC.getContext ghc_session
            GHC.setContext ghc_session ms_mods old_imports

findModule :: ModuleName -> Interpreter GHC.Module
findModule mn =
    do
        ghc_session <- fromSessionState ghcSession
        --
        let mod_name = GHC.mkModuleName mn
        mapGhcExceptions NotAllowed $ GHC.findModule ghc_session mod_name Nothing

mapGhcExceptions :: (String -> InterpreterError) -> IO a -> Interpreter a
mapGhcExceptions buildEx action =
    do
        r <- liftIO $ tryJust ghcExceptions action
        either (throwError . buildEx . flip GHC.showGhcException []) return r

ghcExceptions :: Exception -> Maybe GHC.GhcException
ghcExceptions (DynException a) = fromDynamic a
ghcExceptions  _               = Nothing

-- | Sets the modules whose exports must be in context
setImports :: [ModuleName] -> Interpreter ()
setImports ms =
    do
        ghc_session <- fromSessionState ghcSession
        --
        ms_mods <- mapM findModule ms
        --
        liftIO $ do
            (old_top_level, _) <- GHC.getContext ghc_session
            GHC.setContext ghc_session old_top_level ms_mods

-- | All imported modules are cleared from the context, and
--   loaded modules are unloaded. It is similar to a ":load" in
--   GHCi, but observe that not even the Prelude will be in
--   context after a reset
reset :: Interpreter ()
reset =
    do
        ghc_session <- fromSessionState ghcSession
        --
        -- Remove all modules from context
        liftIO $ GHC.setContext ghc_session [] []
        --
        -- Unload all previously loaded modules
        liftIO $ GHC.setTargets ghc_session []
        liftIO $ GHC.load ghc_session GHC.LoadAllTargets
        --
        -- At this point, GHCi would call rts_revertCAFs and
        -- reset the buffering of stdin, stdout and stderr.
        -- Should we do any of these?
        --
        -- liftIO $ rts_revertCAFs
        --
        return ()


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

-- TODO: Make eval work even if Prelude is not in context!
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
                           -- parsing failed, so we report it just as all
                           -- other errors get reported....
                           logger <- fromSessionState ghcErrLogger
                           liftIO $ logger GHC.SevError span GHC.O.defaultErrStyle err
                           --
                           -- behave like the rest of the GHC API functions
                           -- do on error...
                           return Nothing
        --
        -- "may Have Already Failed", actually :)
        mayFail (return res)

isSucceeded :: GHC.SuccessFlag -> Bool
isSucceeded GHC.Succeeded = True
isSucceeded GHC.Failed    = False

onCompilationError :: ([GhcError] -> Interpreter a) -> (InterpreterError -> Interpreter a)
onCompilationError recover = \interp_error -> case interp_error of
                                                  WontCompile errs -> recover errs
                                                  otherErr         -> throwError otherErr

-- SHOULD WE CALL THIS WHEN MODULES ARE LOADED / UNLOADED?
-- foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()


-- useful when debugging...
mySession = newSessionWith "/opt/local/lib/ghc-6.6"

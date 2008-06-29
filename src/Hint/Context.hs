module Hint.Context (

      ModuleName,
      loadModules, getLoadedModules, setTopLevelModules,
      setImports,
      reset

)

where

import Data.List

import Control.Monad.Error

import Hint.Base
import Hint.Conversions

import qualified GHC

-- | Tries to load all the requested modules from their source file.
--   Modules my be indicated by their ModuleName (e.g. \"My.Module\") or
--   by the full path to its source file.
--
-- The interpreter is 'reset' both before loading the modules and in the event
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

-- | Returns the list of modules loaded with 'loadModules'.
getLoadedModules :: Interpreter [ModuleName]
getLoadedModules = liftM (map modNameFromSummary) getLoadedModSummaries

modNameFromSummary :: GHC.ModSummary -> ModuleName
modNameFromSummary =  fromGhcRep_ . GHC.ms_mod

getLoadedModSummaries :: Interpreter [GHC.ModSummary]
getLoadedModSummaries =
  do ghc_session  <- fromSessionState ghcSession
     --
     all_mod_summ <- liftIO $ GHC.getModuleGraph ghc_session
     filterM (liftIO . GHC.isLoaded ghc_session . GHC.ms_mod_name) all_mod_summ

-- | Sets the modules whose context is used during evaluation. All bindings
--   of these modules are in scope, not only those exported.
--
--   Modules must be interpreted to use this function.
setTopLevelModules :: [ModuleName] -> Interpreter ()
setTopLevelModules ms =
    do
        ghc_session <- fromSessionState ghcSession
        --
        loaded_mods_ghc <- getLoadedModSummaries
        --
        let not_loaded = ms \\ map modNameFromSummary loaded_mods_ghc
        when (not . null $ not_loaded) $
            throwError $ NotAllowed ("These modules have not been loaded:\n" ++
                                     unlines not_loaded)
        --
        ms_mods <- mapM findModule ms
        --
        let mod_is_interpr = GHC.moduleIsInterpreted ghc_session
        not_interpreted <- liftIO $ filterM (liftM not . mod_is_interpr) ms_mods
        when (not . null $ not_interpreted) $
            throwError $ NotAllowed ("These modules are not interpreted:\n" ++
                                     unlines (map fromGhcRep_ not_interpreted))
        --
        liftIO $ do
            (_, old_imports) <- GHC.getContext ghc_session
            GHC.setContext ghc_session ms_mods old_imports

-- | Sets the modules whose exports must be in context.
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
--   loaded modules are unloaded. It is similar to a @:load@ in
--   GHCi, but observe that not even the Prelude will be in
--   context after a reset.
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

-- SHOULD WE CALL THIS WHEN MODULES ARE LOADED / UNLOADED?
-- foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()

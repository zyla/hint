module Hint.Context (

      ModuleName,
      loadModules, getLoadedModules, setTopLevelModules,
      setImports, setImportsQ,
      reset,

      PhantomModule(..), ModuleText,
      addPhantomModule, removePhantomModule, getPhantomModules,

      allModulesInContext, onAnEmptyContext
)

where

import Prelude hiding ( mod )

import Data.Char
import Data.List
import Data.Maybe

import Control.Monad       ( liftM, filterM, when, guard )
import Control.Monad.Error ( catchError, throwError, liftIO )

import Hint.Base
import Hint.Util ( (>=>) ) -- compat version
import Hint.Conversions
import qualified Hint.Util as Util

import qualified GHC
import qualified DriverPhases as DP

import System.Random
import System.FilePath
import System.Directory
import qualified System.IO.UTF8 as UTF8 (writeFile)

type ModuleText = String

-- When creating a phantom module we have a situation similar to that of
-- @Hint.Util.safeBndFor@: we want to avoid picking a module name that is
-- already in-scope. Additionally, since this may be used with sandboxing in
-- mind we want to avoid easy-to-guess names. Thus, we do a trick similar
-- to the one in safeBndFor, but including a random number instead of an
-- additional digit
newPhantomModule :: Interpreter PhantomModule
newPhantomModule =
    do n <- liftIO randomIO :: Interpreter Int
       (ls,is) <- allModulesInContext
       let nums = concat [show (abs n), filter isDigit $ concat (ls ++ is)]
       let mod_name = 'M':nums
       --
       tmp_dir <- liftIO getTemporaryDirectory
       --
       return PhantomModule{pm_name = mod_name, pm_file = tmp_dir </> nums}

allModulesInContext :: Interpreter ([ModuleName], [ModuleName])
allModulesInContext =
    do ghc_session <- fromSessionState ghcSession
       (l, i) <- liftIO $ GHC.getContext ghc_session
       return (map fromGhcRep_ l, map fromGhcRep_ i)

addPhantomModule :: (ModuleName -> ModuleText) -> Interpreter PhantomModule
addPhantomModule mod_text =
    do ghc_session <- fromSessionState ghcSession
       --
       pm <- newPhantomModule
       let t  = fileTarget (pm_file pm)
           m  = GHC.mkModuleName (pm_name pm)
       --
       liftIO $ UTF8.writeFile (pm_file pm) (mod_text $ pm_name pm)
       --
       onState (\s -> s{active_phantoms = pm:active_phantoms s})
       mayFail (do -- GHC.load will remove all the modules from scope, so first
                   -- we save the context...
                   (old_top, old_imps) <- GHC.getContext ghc_session
                   --
                   GHC.addTarget ghc_session t
                   res <- GHC.load ghc_session (GHC.LoadUpTo m)
                   --
                   if isSucceeded res
                     then do GHC.setContext ghc_session old_top old_imps
                             return $ Just ()
                     else return Nothing)
        `catchError` (\err -> case err of
                                WontCompile _ -> do removePhantomModule pm
                                                    throwError err
                                _             -> throwError err)
       --
       return pm

removePhantomModule :: PhantomModule -> Interpreter ()
removePhantomModule pm =
    do ghc_session <- fromSessionState ghcSession
       --
       -- We don't want to actually unload this module, because that
       -- would mean that all the real modules might get reloaded and the
       -- user didn't require that (they may be in a non-compiling state!).
       -- However, this means that we can't actually delete the file, because
       -- it is an active target. Therefore, we simply take it out of scope
       -- and mark it as "delete me when possible" (i.e., next time the
       -- @loadModules@ function is called).
       --
       mod <- findModule (pm_name pm)
       mods' <- liftIO $ do
           (mods, imps) <- GHC.getContext ghc_session
           let mods' = filter (mod /=) mods
           GHC.setContext ghc_session mods' imps
           --
           GHC.removeTarget ghc_session (targetId . fileTarget $ pm_file pm)
           return mods'
       --
       onState (\s -> s{active_phantoms = filter (pm /=) $ active_phantoms s})
       --
       let isNotPhantom = isPhantomModule . fromGhcRep_  >=> return . not
       non_phantoms <- filterM isNotPhantom mods'
       if null non_phantoms
         then do mayFail $ do res <- GHC.load ghc_session GHC.LoadAllTargets
                              return $ guard (isSucceeded res) >> Just ()
                 liftIO $ removeFile (pm_file pm)
         else do onState (\s -> s{zombie_phantoms = pm:zombie_phantoms s})
                 return ()

fileTarget :: FilePath -> GHC.Target
fileTarget f = GHC.Target (GHC.TargetFile f $ Just next_phase) Nothing
    where next_phase = DP.Cpp DP.HsSrcFile

targetId :: GHC.Target -> GHC.TargetId
targetId (GHC.Target _id _) = _id

-- Returns a tuple with the active and zombie phantom modules respectively
getPhantomModules :: Interpreter ([PhantomModule], [PhantomModule])
getPhantomModules = do active <- fromState active_phantoms
                       zombie <- fromState zombie_phantoms
                       return (active, zombie)

isPhantomModule :: ModuleName -> Interpreter Bool
isPhantomModule mn = do (as,zs) <- getPhantomModules
                        return $ mn `elem` (map pm_name $ as ++ zs)

-- | Tries to load all the requested modules from their source file.
--   Modules my be indicated by their ModuleName (e.g. \"My.Module\") or
--   by the full path to its source file.
--
-- The interpreter is 'reset' both before loading the modules and in the event
-- of an error.
loadModules :: [String] -> Interpreter ()
loadModules fs = do -- first, unload everything, and do some clean-up
                    reset
                    doLoad fs `catchError` (\e -> reset >> throwError e)

doLoad :: [String] -> Interpreter ()
doLoad fs = do ghc_session <- fromSessionState ghcSession
               mayFail $ do
                   targets <- mapM (\f -> GHC.guessTarget f Nothing) fs
                   --
                   GHC.setTargets ghc_session targets
                   res <- GHC.load ghc_session GHC.LoadAllTargets
                   return $ guard (isSucceeded res) >> Just ()

-- | Returns the list of modules loaded with 'loadModules'.
getLoadedModules :: Interpreter [ModuleName]
getLoadedModules = do (active_pms, zombie_pms) <- getPhantomModules
                      ms <- map modNameFromSummary `liftM` getLoadedModSummaries
                      return $ ms \\ (map pm_name $ active_pms ++ zombie_pms)

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
        active_pms <- fromState active_phantoms
        ms_mods <- mapM findModule (nub $ ms ++ map pm_name active_pms)
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

onAnEmptyContext :: Interpreter a -> Interpreter a
onAnEmptyContext action =
    do ghc_session <- fromSessionState ghcSession
       (old_mods, old_imps) <- liftIO $ GHC.getContext ghc_session
       liftIO $ GHC.setContext ghc_session [] []
       let restore = liftIO $ GHC.setContext ghc_session old_mods old_imps
       a <- action `catchError` (\e -> do restore; throwError e)
       restore
       return a

-- | Sets the modules whose exports must be in context.
setImports :: [ModuleName] -> Interpreter ()
setImports ms = setImportsQ $ zip ms (repeat Nothing)

-- | Sets the modules whose exports must be in context; some
--   of them may be qualified. E.g.:
--
--   @setImports [("Prelude", Nothing), ("Data.Map", Just "M")]@.
--
--   Here, "map" will refer to Prelude.map and "M.map" to Data.Map.map.
setImportsQ :: [(ModuleName, Maybe String)] -> Interpreter ()
setImportsQ ms =
    do ghc_session <- fromSessionState ghcSession
       --
       let (q,     u) = Util.partition (isJust . snd) ms
           (quals, unquals) = (map (\(a, Just b) -> (a,b)) q, map fst u)
       --
       unqual_mods <- mapM findModule unquals
       mapM_ (findModule . fst) quals -- just to be sure they exist
       --
       old_qual_hack_mod <- fromState import_qual_hack_mod
       maybe (return ()) removePhantomModule old_qual_hack_mod
       --
       new_pm <- if ( not $ null quals )
                   then do
                     new_pm <- addPhantomModule $ \mod_name -> unlines $
                                ("module " ++ mod_name ++ " where ") :
                                ["import qualified " ++ m ++ " as " ++ n |
                                   (m,n) <- quals]
                     onState (\s -> s{import_qual_hack_mod = Just new_pm})
                     return $ Just new_pm
                   else return Nothing
       --
       pm <- maybe (return []) (findModule . pm_name >=> return . return) new_pm
       liftIO $ do
           (old_top_level, _) <- GHC.getContext ghc_session
           let new_top_level = pm ++ old_top_level
           GHC.setContext ghc_session new_top_level unqual_mods
       --
       onState (\s ->s{qual_imports = quals})

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
        -- We now remove every phantom module and forget about qual imports
        old_active <- fromState active_phantoms
        old_zombie <- fromState zombie_phantoms
        onState (\s -> s{active_phantoms      = [],
                         zombie_phantoms      = [],
                         import_qual_hack_mod = Nothing,
                         qual_imports         = []})
        liftIO $ mapM_ (removeFile . pm_file) (old_active ++ old_zombie)

-- SHOULD WE CALL THIS WHEN MODULES ARE LOADED / UNLOADED?
-- foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()

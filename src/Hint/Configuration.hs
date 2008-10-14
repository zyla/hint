module Hint.Configuration (

      setGhcOption, setGhcOptions,

      setUseLanguageExtensions,
      Optimizations(..), setOptimizations,

      setInstalledModsAreInScopeQualified ) where

import Control.Monad.Error
import qualified GHC
import Hint.Base

setGhcOptions :: MonadInterpreter m => [String] -> m ()
setGhcOptions opts =
    do ghc_session <- fromSession ghcSession
       old_flags   <- liftIO $ GHC.getSessionDynFlags ghc_session
       (new_flags, not_parsed) <- liftIO $ GHC.parseDynamicFlags old_flags opts
       when (not . null $ not_parsed) $
            throwError $ UnknownError (concat ["flag: '", unwords opts,
                                               "' not recognized"])
       liftIO $ GHC.setSessionDynFlags ghc_session new_flags
       return ()

setGhcOption :: MonadInterpreter m => String -> m ()
setGhcOption opt = setGhcOptions [opt]

-- | Set to true to allow GHC's extensions to Haskell 98.
setUseLanguageExtensions :: MonadInterpreter m => Bool -> m ()
setUseLanguageExtensions True  = do setGhcOption "-fglasgow-exts"
                                    setGhcOption "-fextended-default-rules"
setUseLanguageExtensions False = do setGhcOption "-fno-glasgow-exts"
                                    setGhcOption "-fno-extended-default-rules"

data Optimizations = None | Some | All deriving (Eq, Read, Show)

-- | Set the optimization level (none, some, all)
setOptimizations :: MonadInterpreter m => Optimizations -> m ()
setOptimizations None = setGhcOption "-O0"
setOptimizations Some = setGhcOption "-O1"
setOptimizations All  = setGhcOption "-O2"

-- | When set to @True@, every module in every available package is implicitly
--   imported qualified. This is very convenient for interactive
--   evaluation, but can be a problem in sandboxed environments
--   (e.g. 'System.Unsafe.unsafePerformIO' is in scope').
--
--   Default value is @True@.
--
--   Observe that due to limitations in the GHC-API, when set to @False@, the
--   private symbols in interpreted modules will not be in scope.
setInstalledModsAreInScopeQualified :: MonadInterpreter m => Bool -> m ()
setInstalledModsAreInScopeQualified b = onState $ \s -> s{all_mods_in_scope = b}

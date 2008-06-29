module Hint.Configuration (

      setGhcOption, setGhcOptions,

      setUseLanguageExtensions,
      Optimizations(..), setOptimizations

)

where

import Control.Monad.Error
import qualified GHC
import Hint.Base

setGhcOptions :: [String] -> Interpreter ()
setGhcOptions opts =
    do ghc_session <- fromSessionState ghcSession
       old_flags   <- liftIO $ GHC.getSessionDynFlags ghc_session
       (new_flags, not_parsed) <- liftIO $ GHC.parseDynamicFlags old_flags opts
       when (not . null $ not_parsed) $
            throwError $ UnknownError (concat ["flag: '", unwords opts,
                                               "' not recognized"])
       liftIO $ GHC.setSessionDynFlags ghc_session new_flags
       return ()

setGhcOption :: String -> Interpreter ()
setGhcOption opt = setGhcOptions [opt]


-- | Set to true to allow GHC's extensions to Haskell 98.
setUseLanguageExtensions :: Bool -> Interpreter ()
setUseLanguageExtensions True  = setGhcOption "-fglasgow-exts"
setUseLanguageExtensions False = setGhcOption "-fno-glasgow-exts"

data Optimizations = None | Some | All deriving (Eq, Read, Show)

-- | Set the optimization level (none, some, all)
setOptimizations :: Optimizations -> Interpreter ()
setOptimizations None = setGhcOption "-O0"
setOptimizations Some = setGhcOption "-O1"
setOptimizations All  = setGhcOption "-O2"

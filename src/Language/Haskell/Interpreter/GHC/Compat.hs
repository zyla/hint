module Language.Haskell.Interpreter.GHC.Compat

where

import qualified GHC

#if __GLASGOW_HASKELL__ >= 608

newSession :: FilePath -> IO GHC.Session
newSession ghc_root = GHC.newSession (Just ghc_root)

#elif __GLASGOW_HASKELL__ >= 606

newSession :: FilePath -> IO GHC.Session
newSession ghc_root = GHC.newSession GHC.Interactive (Just ghc_root)

#endif
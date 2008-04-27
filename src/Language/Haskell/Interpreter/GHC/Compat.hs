module Language.Haskell.Interpreter.GHC.Compat

where

import qualified GHC
import qualified Pretty
import qualified Outputable

#if __GLASGOW_HASKELL__ >= 608

import qualified PprTyThing

#endif

-- Kinds became a synonym for Type in GHC 6.8. We define this wrapper
-- to be able to define a FromGhcRep instance for both versions
newtype Kind = Kind GHC.Kind

#if __GLASGOW_HASKELL__ >= 608

newSession :: FilePath -> IO GHC.Session
newSession ghc_root = GHC.newSession (Just ghc_root)

pprType :: GHC.Type -> (Outputable.PprStyle -> Pretty.Doc)
pprType = PprTyThing.pprTypeForUser False -- False means drop explicit foralls

pprKind :: GHC.Kind -> (Outputable.PprStyle -> Pretty.Doc)
pprKind = pprType

#elif __GLASGOW_HASKELL__ >= 606

newSession :: FilePath -> IO GHC.Session
newSession ghc_root = GHC.newSession GHC.Interactive (Just ghc_root)

pprType :: GHC.Type -> (Outputable.PprStyle -> Pretty.Doc)
pprType = Outputable.ppr . GHC.dropForAlls

pprKind :: GHC.Kind -> (Outputable.PprStyle -> Pretty.Doc)
pprKind = Outputable.ppr

#endif
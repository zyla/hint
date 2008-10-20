module Hint.Compat

where

import qualified Hint.GHC as GHC

-- Kinds became a synonym for Type in GHC 6.8. We define this wrapper
-- to be able to define a FromGhcRep instance for both versions
newtype Kind = Kind GHC.Kind

#if __GLASGOW_HASKELL__ >= 608

#if __GLASGOW_HASKELL__ < 610
newSession :: FilePath -> IO GHC.Session
newSession ghc_root =
    do s <- GHC.newSession (Just ghc_root)
       dflags <- GHC.getSessionDynFlags s
       GHC.setSessionDynFlags s dflags{GHC.ghcMode    = GHC.CompManager,
                                       GHC.hscTarget  = GHC.HscInterpreted,
                                       GHC.ghcLink    = GHC.LinkInMemory}
       return s
#endif

pprType :: GHC.Type -> (GHC.PprStyle -> GHC.Doc)
pprType = GHC.pprTypeForUser False -- False means drop explicit foralls

pprKind :: GHC.Kind -> (GHC.PprStyle -> GHC.Doc)
pprKind = pprType

#elif __GLASGOW_HASKELL__ >= 606

newSession :: FilePath -> IO GHC.Session
newSession ghc_root =
    do s <- GHC.newSession GHC.Interactive (Just ghc_root)
       dflags <- GHC.getSessionDynFlags s
       GHC.setSessionDynFlags s dflags{GHC.hscTarget  = GHC.HscInterpreted}
       return s

pprType :: GHC.Type -> (GHC.PprStyle -> GHC.Doc)
pprType = GHC.ppr . GHC.dropForAlls

pprKind :: GHC.Kind -> (GHC.PprStyle -> GHC.Doc)
pprKind = GHC.ppr

#endif


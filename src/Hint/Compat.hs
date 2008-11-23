module Hint.Compat

where

import qualified Hint.GHC as GHC

-- Kinds became a synonym for Type in GHC 6.8. We define this wrapper
-- to be able to define a FromGhcRep instance for both versions
newtype Kind = Kind GHC.Kind

#if __GLASGOW_HASKELL__ >= 610
parseDynamicFlags :: GHC.GhcMonad m
                   => GHC.DynFlags -> [String] -> m (GHC.DynFlags, [String])
parseDynamicFlags d = fmap firstTwo . GHC.parseDynamicFlags d . map GHC.noLoc
    where firstTwo (a,b,_) = (a, map GHC.unLoc b)

fileTarget :: FilePath -> GHC.Target
fileTarget f = GHC.Target (GHC.TargetFile f $ Just next_phase) True Nothing
    where next_phase = GHC.Cpp GHC.HsSrcFile

targetId :: GHC.Target -> GHC.TargetId
targetId = GHC.targetId

guessTarget :: GHC.GhcMonad m => String -> Maybe GHC.Phase -> m GHC.Target
guessTarget = GHC.guessTarget

#else
-- add a bogus session parameter, in order to use it with runGhc2
parseDynamicFlags :: GHC.Session
                  -> GHC.DynFlags
                  -> [String] -> IO (GHC.DynFlags, [String])
parseDynamicFlags = const GHC.parseDynamicFlags

fileTarget :: FilePath -> GHC.Target
fileTarget f = GHC.Target (GHC.TargetFile f $ Just next_phase) Nothing
    where next_phase = GHC.Cpp GHC.HsSrcFile

targetId :: GHC.Target -> GHC.TargetId
targetId (GHC.Target _id _) = _id

-- add a bogus session parameter, in order to use it with runGhc2
guessTarget :: GHC.Session -> String -> Maybe GHC.Phase -> IO GHC.Target
guessTarget = const GHC.guessTarget
#endif

#if __GLASGOW_HASKELL__ >= 608
#if __GLASGOW_HASKELL__ < 610
  -- 6.08 only
newSession :: FilePath -> IO GHC.Session
newSession ghc_root =
    do s <- GHC.newSession (Just ghc_root)
       dflags <- GHC.getSessionDynFlags s
       GHC.setSessionDynFlags s dflags{GHC.ghcMode    = GHC.CompManager,
                                       GHC.hscTarget  = GHC.HscInterpreted,
                                       GHC.ghcLink    = GHC.LinkInMemory}
       return s
#endif

  -- 6.08 and above
pprType :: GHC.Type -> (GHC.PprStyle -> GHC.Doc)
pprType = GHC.pprTypeForUser False -- False means drop explicit foralls

pprKind :: GHC.Kind -> (GHC.PprStyle -> GHC.Doc)
pprKind = pprType

#elif __GLASGOW_HASKELL__ >= 606
  -- 6.6 only

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


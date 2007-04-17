{-# OPTIONS_GHC -fglasgow-exts #-}
module Language.Haskell.Interpreter.GHC.Conversions(GhcToHs(..))

where

import qualified GHC        as GHC  (Type, PrintUnqualified, alwaysQualify)
import qualified Outputable as GHC.O(Outputable(ppr), showSDocForUser)

import Language.Haskell.Syntax(HsModule(..), HsDecl(..), HsQualType)
import Language.Haskell.Parser(parseModule, ParseResult(ParseOk))

-- | Conversions from GHC representation to Language.Haskell.Syntax representation
class GhcToHs ghc hs_src where
    ghc2hs :: ghc -> hs_src

instance GhcToHs GHC.Type HsQualType where
    ghc2hs t = ghc2hs (t, GHC.alwaysQualify)

instance GhcToHs (GHC.Type, GHC.PrintUnqualified) HsQualType where
    ghc2hs (t, p) = qualType
        where HsModule  _ _ _ _ [decl,_] = parseModule' $ unlines ["f ::" ++ strQual p t,
                                                                   "f = undefined"]
              HsTypeSig _ _ qualType     = decl

parseModule' :: String -> HsModule
parseModule' s = case parseModule s of
                    ParseOk m -> m
                    failed    -> error $ unlines ["parseModulde' failed?!", s, show failed]

strQual :: GHC.O.Outputable a => GHC.PrintUnqualified -> a -> String
strQual unqual = GHC.O.showSDocForUser unqual . GHC.O.ppr


{-# OPTIONS_GHC -fglasgow-exts #-}
module Language.Haskell.Interpreter.GHC.Conversions(FromGhcRep(..))

where

import qualified GHC        as GHC  (Type, Kind, PrintUnqualified, alwaysQualify)
import qualified Outputable as GHC.O(Outputable(ppr), showSDoc, showSDocForUser)

import Language.Haskell.Syntax(HsModule(..), HsDecl(..), HsQualType)
import Language.Haskell.Parser(parseModule, ParseResult(ParseOk))

-- | Conversions from GHC representation to standard representations
class FromGhcRep ghc target where
    fromGhcRep :: ghc -> target

-- --------- Types -----------------------
instance FromGhcRep GHC.Type HsQualType where
    fromGhcRep t = fromGhcRep (t, GHC.alwaysQualify)

instance FromGhcRep (GHC.Type, GHC.PrintUnqualified) HsQualType where
    fromGhcRep (t, p) = qualType
        where HsModule  _ _ _ _ [decl,_] = parseModule' $ unlines ["f ::" ++ fromGhcRep (t,p),
                                                                   "f = undefined"]
              HsTypeSig _ _ qualType     = decl

instance FromGhcRep (GHC.Type, GHC.PrintUnqualified) String where
    fromGhcRep (t, p) = GHC.O.showSDocForUser p . GHC.O.ppr $ t

parseModule' :: String -> HsModule
parseModule' s = case parseModule s of
                    ParseOk m -> m
                    failed    -> error $ unlines ["parseModulde' failed?!", s, show failed]


-- --------------------- Kinds -----------------

instance FromGhcRep GHC.Kind String where
    fromGhcRep k = GHC.O.showSDoc (GHC.O.ppr k)
module Hint.GHC.Conversions( FromGhcRep(..), FromGhcRep_(..), isSucceeded )

where

import Control.Monad.Trans ( liftIO )

import qualified GHC        as GHC
import qualified Outputable as GHC.O

import Hint.GHC.Base
import qualified Hint.GHC.Compat as Compat

import Language.Haskell.Syntax ( HsModule(..), HsDecl(..), HsQualType )
import Language.Haskell.Parser ( parseModule, ParseResult(ParseOk) )

-- | Conversions from GHC representation to standard representations
class FromGhcRep ghc target where
    fromGhcRep :: ghc -> Interpreter target

-- --------- Types / Kinds -----------------------

instance FromGhcRep GHC.Type HsQualType where
    fromGhcRep t =
        do t_str <- fromGhcRep t
           --
           let mod_str = unlines ["f ::" ++ t_str,
                                  "f = undefined"]
           let HsModule  _ _ _ _ [decl,_] = parseModule' mod_str
               HsTypeSig _ _ qualType     = decl
           --
           return qualType


instance FromGhcRep GHC.Type String where
    fromGhcRep t = do ghc_session <- fromSessionState ghcSession
                      -- Unqualify necessary types
                      -- (i.e., do not expose internals)
                      unqual <- liftIO $ GHC.getPrintUnqual ghc_session
                      return $ GHC.O.showSDocForUser unqual (Compat.pprType t)

parseModule' :: String -> HsModule
parseModule' s = case parseModule s of
                    ParseOk m -> m
                    failed    -> error $ unlines ["parseModulde' failed?!",
                                                  s,
                                                  show failed]

instance FromGhcRep Compat.Kind String where
    fromGhcRep (Compat.Kind k) = return $ GHC.O.showSDoc (Compat.pprKind k)


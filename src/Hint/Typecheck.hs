module Hint.Typecheck (
      typeOf, typeChecks, kindOf, normalizeType
) where

import Control.Monad.Catch

import Hint.Base
import Hint.Parsers
import Hint.Conversions

import qualified Hint.Compat as Compat
import qualified Hint.GHC as GHC

-- | Returns a string representation of the type of the expression.
typeOf :: MonadInterpreter m => String -> m String
typeOf expr =
    do -- First, make sure the expression has no syntax errors,
       -- for this is the only way we have to "intercept" this
       -- kind of errors
       failOnParseError parseExpr expr
       --
       ty <- mayFail $ runGhc1 exprType expr
       --
       typeToString ty

-- | Tests if the expression type checks.
typeChecks :: MonadInterpreter m => String -> m Bool
typeChecks expr = (typeOf expr >> return True)
                              `catchIE`
                              onCompilationError (\_ -> return False)

-- | Returns a string representation of the kind of the type expression.
kindOf :: MonadInterpreter m => String -> m String
kindOf type_expr =
    do -- First, make sure the expression has no syntax errors,
       -- for this is the only way we have to "intercept" this
       -- kind of errors
       failOnParseError parseType type_expr
       --
       (_, kind) <- mayFail $ runGhc1 typeKind type_expr
       --
       kindToString (Compat.Kind kind)

-- | Returns a string representation of the normalized type expression.
-- This is what @:kind!@ GHCi command prints after @=@.
normalizeType :: MonadInterpreter m => String -> m String
normalizeType type_expr =
    do -- First, make sure the expression has no syntax errors,
       -- for this is the only way we have to "intercept" this
       -- kind of errors
       failOnParseError parseType type_expr
       --
       (ty, _) <- mayFail $ runGhc1 typeKind type_expr
       --
       typeToString ty

-- add a bogus Maybe, in order to use it with mayFail
exprType :: GHC.GhcMonad m => String -> m (Maybe GHC.Type)
exprType = fmap Just . GHC.exprType

-- add a bogus Maybe, in order to use it with mayFail
typeKind :: GHC.GhcMonad m => String -> m (Maybe (GHC.Type, GHC.Kind))
typeKind = fmap Just . GHC.typeKind True

onCompilationError :: MonadInterpreter m
                   => ([GhcError] -> m a)
                   -> (InterpreterError -> m a)
onCompilationError recover interp_error
    = case interp_error of
          WontCompile errs -> recover errs
          otherErr         -> throwM otherErr

module Hint.GHC.Typecheck (

      typeOf, typeChecks, kindOf

)

where

import Control.Monad.Error

import Hint.GHC.Base
import Hint.GHC.Parsers
import Hint.GHC.Conversions

import qualified Hint.GHC.Compat as Compat

import qualified GHC

-- | Returns a string representation of the type of the expression.
typeOf :: String -> Interpreter String
typeOf expr =
    do
        ghc_session <- fromSessionState ghcSession
        --
        -- First, make sure the expression has no syntax errors,
        -- for this is the only way we have to "intercept" this
        -- kind of errors
        failOnParseError parseExpr expr
        --
        ty <- mayFail $ GHC.exprType ghc_session expr
        --
        fromGhcRep ty


-- | Tests if the expression type checks.
typeChecks :: String -> Interpreter Bool
typeChecks expr = (typeOf expr >> return True)
                  `catchError`
                  onCompilationError (\_ -> return False)

-- | Returns a string representation of the kind of the type expression.
kindOf :: String -> Interpreter String
kindOf type_expr =
    do
        ghc_session <- fromSessionState ghcSession
        --
        -- First, make sure the expression has no syntax errors,
        -- for this is the only way we have to "intercept" this
        -- kind of errors
        failOnParseError parseType type_expr

        kind <- mayFail $ GHC.typeKind ghc_session type_expr
        --
        return $ fromGhcRep_ (Compat.Kind kind)


onCompilationError :: ([GhcError] -> Interpreter a)
                   -> (InterpreterError -> Interpreter a)
onCompilationError recover =
    \interp_error -> case interp_error of
                       WontCompile errs -> recover errs
                       otherErr         -> throwError otherErr

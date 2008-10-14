module Hint.Typecheck (

      typeOf, typeChecks, kindOf,

      typeOf_unsandboxed, typeChecks_unsandboxed

)

where

import Control.Monad.Error

import Hint.Base
import Hint.Parsers
import Hint.Conversions
import Hint.Sandbox

import qualified Hint.Compat as Compat

import qualified GHC

-- | Returns a string representation of the type of the expression.
typeOf :: String -> Interpreter String
typeOf = sandboxed typeOf_unsandboxed

typeOf_unsandboxed :: String -> Interpreter String
typeOf_unsandboxed expr =
    do
        ghc_session <- fromSession ghcSession
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
typeChecks = sandboxed typeChecks_unsandboxed

typeChecks_unsandboxed :: String -> Interpreter Bool
typeChecks_unsandboxed expr = (typeOf_unsandboxed expr >> return True)
                              `catchError`
                              onCompilationError (\_ -> return False)

-- | Returns a string representation of the kind of the type expression.
kindOf :: String -> Interpreter String
kindOf = sandboxed go
    where go type_expr =
              do ghc_session <- fromSession ghcSession
                 --
                 -- First, make sure the expression has no syntax errors,
                 -- for this is the only way we have to "intercept" this
                 -- kind of errors
                 failOnParseError parseType type_expr
                 --
                 kind <- mayFail $ GHC.typeKind ghc_session type_expr
                 --
                 return $ fromGhcRep_ (Compat.Kind kind)

onCompilationError :: ([GhcError] -> Interpreter a)
                   -> (InterpreterError -> Interpreter a)
onCompilationError recover =
    \interp_error -> case interp_error of
                       WontCompile errs -> recover errs
                       otherErr         -> throwError otherErr

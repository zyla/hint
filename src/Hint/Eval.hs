module Hint.Eval (

      interpret, as, infer,
      eval )

where

import qualified GHC
import qualified GHC.Exts ( unsafeCoerce# )

import Data.Typeable hiding ( typeOf )
import qualified Data.Typeable ( typeOf )

import Hint.Base
import Hint.Parsers
import Hint.Sandbox
import Hint.Util


-- | Convenience functions to be used with @interpret@ to provide witnesses.
--   Example:
--
--   * @interpret \"head [True,False]\" (as :: Bool)@
--
--   * @interpret \"head $ map show [True,False]\" infer >>= flip interpret (as :: Bool)@
as, infer :: Typeable a => a
as    = undefined
infer = undefined

-- | Evaluates an expression, given a witness for its monomorphic type.
interpret :: Typeable a => String -> a -> Interpreter a
interpret expr witness = sandboxed go expr
  where go e =
         do ghc_session <- fromSessionState ghcSession
            --
            -- First, make sure the expression has no syntax errors,
            -- for this is the only way we have to "intercept" this
            -- kind of errors
            failOnParseError parseExpr e
            --
            let expr_typesig = concat [parens e," :: ",show $ myTypeOf witness]
            expr_val <- mayFail $ GHC.compileExpr ghc_session expr_typesig
            --
            return (GHC.Exts.unsafeCoerce# expr_val :: a)

-- HACK! Allows evaluations even when the Prelude is not in scope
myTypeOf :: Typeable a => a -> TypeRep
myTypeOf a
    | type_of_a == type_of_string = qual_type_of_string
    | otherwise                   = type_of_a
    where type_of_a           = Data.Typeable.typeOf a
          type_of_string      = Data.Typeable.typeOf (undefined :: [Char])
          (list_ty_con, _)    = splitTyConApp type_of_string
          qual_type_of_string = mkTyConApp list_ty_con
                                        [mkTyConApp (mkTyCon "Prelude.Char") []]

-- | @eval expr@ will evaluate @show expr@.
--  It will succeed only if @expr@ has type t and there is a 'Show'
--  instance for t.
eval :: String -> Interpreter String
eval expr = interpret show_expr (as :: String)
    where show_expr =  "Prelude.show" ++ (parens expr)


-- Conceptually, @parens s = "(" ++ s ++ ")"@, where s is some valid haskell
-- expression. In practice, it is harder than this.
-- Observe that if @s@ ends with a trailing comment, then @parens s@ would
-- be a malformed expression. The straightforward solution for this is to
-- put the closing parenthesis in a different line. However, now we are
-- messing with the layout rules and we don't know where @s@ is going to
-- be used!
-- Solution: @parens s = "(let {foo = " ++ s ++
--                       "  ;} in foo)@ where foo does not occur in s
parens :: String -> String
parens s = concat ["(let {", foo, " = ", s, "\n",
                    "                     ;} in ", foo, ")"]
    where foo = safeBndFor s

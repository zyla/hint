module Hint.Sandbox ( sandboxed ) where

import Hint.Base
import Hint.Util

sandboxed :: MonadInterpreter m => (Expr -> m a) -> (Expr -> m a)
sandboxed = id

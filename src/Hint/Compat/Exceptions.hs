module Hint.Compat.Exceptions (
#if __GLASGOW_HASKELL__ >= 610
  module Control.Monad.CatchIO
#else
  throw, catch,
  module Control.Monad.CatchIO.Old
#endif

)

where

import Prelude hiding ( catch )

#if __GLASGOW_HASKELL__ >= 610
import Control.Monad.CatchIO
#else
import Data.Dynamic

import Control.Monad.CatchIO.Old hiding ( throw, catch )

throw :: Typeable e => e -> a
throw = throwDyn

catch :: (Typeable e, MonadCatchIO m) => m a -> (e -> m a) -> m a
catch = catchDyn
#endif

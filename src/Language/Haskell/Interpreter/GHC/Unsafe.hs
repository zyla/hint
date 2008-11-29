module Language.Haskell.Interpreter.GHC.Unsafe ( unsafeSetGhcOption )

where

import Hint.Base
import Hint.Configuration

-- | Set a GHC option for the current session,
--   eg. @unsafeSetGhcOption \"-XNoMonomorphismRestriction\"@.
--
--   Warning: Some options may interact badly with the Interpreter.
unsafeSetGhcOption :: MonadInterpreter m => String -> m ()
unsafeSetGhcOption = setGhcOption

module Language.Haskell.Interpreter.GHC.Unsafe ( unsafeSetGhcOption )

where

import Hint.Base
import Hint.Configuration

-- | Set a GHC option for the current session,
--   eg. @unsafeSetGhcOption \"-fno-monomorphism-restriction\"@.
--
--   Warning: Some options may interact badly with the Interpreter.
unsafeSetGhcOption :: String -> Interpreter ()
unsafeSetGhcOption = setGhcOption

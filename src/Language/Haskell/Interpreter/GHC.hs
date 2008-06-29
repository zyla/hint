-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Interpreter.GHC
-- License     :  BSD-style
--
-- Maintainer  :  jcpetruzza@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (GHC API)
--
-- A Haskell interpreter built on top of the GHC API
-----------------------------------------------------------------------------
module Language.Haskell.Interpreter.GHC(
    -- * Session handling
     InterpreterSession, newSession, newSessionUsing,
    -- * Error handling
     InterpreterError(..), GhcError(..),
    -- * The interpreter type
     Interpreter,
    -- ** Running the interpreter
     withSession,
    -- ** Interpreter options
     setUseLanguageExtensions,
     Optimizations(..), setOptimizations,
    -- ** Context handling
     ModuleName,
     loadModules, getLoadedModules, setTopLevelModules,
     setImports,
     reset,
    -- ** Module querying
     ModuleElem(..), Id, name, children,
     getModuleExports,
    -- ** Type inference
     typeOf, typeChecks, kindOf,
    -- ** Evaluation
     interpret, as, infer,
     eval)

where

import Hint.GHC.Base
import Hint.GHC.Configuration
import Hint.GHC.Context
import Hint.GHC.Reflection
import Hint.GHC.Typecheck
import Hint.GHC.Eval
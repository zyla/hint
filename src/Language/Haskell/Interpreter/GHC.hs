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
    -- * The interpreter monad transformer
     MonadInterpreter(..), InterpreterT, Interpreter,
    -- ** Running the interpreter
     runInterpreter,
    -- ** Interpreter options
     setUseLanguageExtensions,
     Optimizations(..), setOptimizations,
     setInstalledModsAreInScopeQualified,
    -- ** Context handling
     ModuleName,
     loadModules, getLoadedModules, setTopLevelModules,
     setImports, setImportsQ,
     reset,
    -- ** Module querying
     ModuleElem(..), Id, name, children,
     getModuleExports,
    -- ** Type inference
     typeOf, typeChecks, kindOf,
    -- ** Evaluation
     interpret, as, infer, eval,
    -- * Error handling
     InterpreterError(..), GhcError(..))

where

import Hint.Base
import Hint.InterpreterT
import Hint.Configuration
import Hint.Context
import Hint.Reflection
import Hint.Typecheck
import Hint.Eval

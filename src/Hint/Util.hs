module Hint.Util where

import Hint.Base
import Hint.Context
import Hint.Conversions

import qualified GHC

import Control.Monad.Trans ( liftIO )

import Data.Char

import System.Directory
import System.FilePath
import System.Random

type Expr = String

-- @safeBndFor expr@ generates a name @e@ such that it does not
-- occur free in @expr@ and, thus, it is safe to write something
-- like @e = expr@ (otherwise, it will get accidentally bound).
-- This ought to do the trick: observe that @safeBndFor expr@
-- contains more digits than @expr@ and, thus, cannot occur inside
-- @expr@.
safeBndFor :: Expr -> String
safeBndFor expr = "e_1" ++ filter isDigit expr

-- We have a similar situation with the module name as we have with
-- the binder name (see safeBndFor): we want to avoid a module that is
-- in-scope. Additionally, since this may be used with sandboxing in mind
-- we want to avoid easy-to-guess names. Thus, we do a trick similar to the
-- one in safeBndFor, but including a random number instead of an additional
-- digit
mkModName :: Interpreter (ModuleName, FilePath)
mkModName =
    do n <- liftIO randomIO :: Interpreter Int
       (ls,is) <- modulesInContext
       let nums = concat [show (abs n), filter isDigit $ concat (ls ++ is)]
       let mod_name = 'M':nums
       --
       tmp_dir <- liftIO getTemporaryDirectory
       --
       return (mod_name, tmp_dir </> nums)

modulesInContext :: Interpreter ([ModuleName], [ModuleName])
modulesInContext =
    do ghc_session <- fromSessionState ghcSession
       (l, i) <- liftIO $ GHC.getContext ghc_session
       return (map fromGhcRep_ l, map fromGhcRep_ i)

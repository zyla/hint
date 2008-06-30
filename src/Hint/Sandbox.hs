module Hint.Sandbox ( sandboxed ) where

import Hint.Base
import Hint.Conversions
import Hint.Context

import {-# SOURCE #-} Hint.Typecheck ( typeChecks_unsandboxed )

import qualified GHC
import qualified DriverPhases as DP

import Data.Char

import Control.Monad
import Control.Monad.Trans

import System.Directory
import System.FilePath
import System.Random

type Expr = String

sandboxed :: (Expr -> Interpreter a) -> (Expr -> Interpreter a)
sandboxed do_stuff = \expr -> do dont_need_sandbox <- fromConf all_mods_in_scope
                                 if dont_need_sandbox
                                   then do_stuff expr
                                   else usingAModule do_stuff expr

usingAModule :: (Expr -> Interpreter a) -> (Expr -> Interpreter a)
usingAModule do_stuff_on = \expr ->
    do ghc_session <- fromSessionState ghcSession
       (mod_name, mod_file) <- mkModName
       --
       -- To avoid defaulting, we will evaluate this expression without the
       -- monomorphism-restriction. This means that expressions that normally
       -- would not typecheck, suddenly will. Thus, we first check if the
       -- expression typechecks as is. If it doesn't, there is no need in
       -- going on (if it does, it may not typecheck once we restrict the
       -- context; that is the whole idea of this!)
       --
       type_checks <- typeChecks_unsandboxed expr
       case type_checks of
         False -> do_stuff_on expr -- fail as you wish...
         True  ->
             do (loaded, imports) <- modulesInContext
                --
                let e = safeBndFor expr
                let mod_text = textify [
                         ["{-# OPTIONS_GHC -fno-monomorphism-restriction #-}"],
                         ["{-# OPTIONS_GHC -fno-implicit-prelude #-}"],
                         ["module " ++ mod_name],
                         ["where"],
                         ["import " ++ m | m <- loaded ++ imports],
                         ["default ()"],
                         [e ++ " = " ++ expr] ]
                liftIO $ writeFile mod_file mod_text
                let t = fileTarget mod_file
                --
                setTopLevelModules []
                setImports []
                mayFail $ do GHC.addTarget ghc_session t
                             res <- GHC.load ghc_session GHC.LoadAllTargets
                             return $ guard (isSucceeded res) >> Just ()
                --
                setTopLevelModules [mod_name]
                r <- do_stuff_on e
                --
                mayFail $ do GHC.removeTarget ghc_session (targetId t)
                             res <- GHC.load ghc_session GHC.LoadAllTargets
                             return $ guard (isSucceeded res) >> Just ()
                setTopLevelModules loaded
                setImports imports
                --
                return r
             `finally`
             clean_up mod_file
           --
           where textify    = unlines . concat
                 clean_up f = liftIO $ do exists <- doesFileExist f
                                          when exists $
                                               removeFile f


targetId :: GHC.Target -> GHC.TargetId
targetId (GHC.Target _id _) = _id

fileTarget :: FilePath -> GHC.Target
fileTarget f = GHC.Target (GHC.TargetFile f $ Just next_phase) Nothing
    where next_phase = DP.Cpp DP.HsSrcFile

-- Since instead of working with expr, we will work with
-- e = expr, we must be sure that e does not occur free in expr
-- (otherwise, it will get accidentally bound). This ought to
-- do the trick: observe that "safeBndFor expr" contains more digits
-- than "expr" and, thus, cannot occur inside "expr".
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

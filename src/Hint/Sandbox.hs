module Hint.Sandbox ( sandboxed ) where

import Hint.Base
import Hint.Conversions
import Hint.Context
import Hint.Util

import {-# SOURCE #-} Hint.Typecheck ( typeChecks_unsandboxed )

import qualified GHC
import qualified DriverPhases as DP

import Control.Monad.Error
import System.Directory
import qualified System.IO.UTF8 as UTF (writeFile)

sandboxed :: (Expr -> Interpreter a) -> (Expr -> Interpreter a)
sandboxed do_stuff = \expr -> do dont_need_sandbox <- fromConf all_mods_in_scope
                                 if dont_need_sandbox
                                   then do_stuff expr
                                   else usingAModule do_stuff expr

usingAModule :: (Expr -> Interpreter a) -> (Expr -> Interpreter a)
usingAModule do_stuff_on = \expr ->
    do (mod_name, mod_file) <- mkModName
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
                let mod_text no_prel = textify [
                        ["{-# OPTIONS_GHC -fno-monomorphism-restriction #-}"],
                        ["{-# OPTIONS_GHC -fno-implicit-prelude #-}" | no_prel],
                        ["module " ++ mod_name],
                        ["where"],
                        ["import " ++ m | m <- loaded ++ imports],
                        [e ++ " = " ++ expr] ]
                let write_mod = liftIO . UTF.writeFile mod_file . mod_text
                let t = fileTarget mod_file
                --
                setTopLevelModules []
                setImports []
                let go = do addTarget t
                            setTopLevelModules [mod_name]
                            do_stuff_on e
                write_mod True
                -- If the Prelude was not explicitly imported but implicitly
                -- imported in some interpreted module, then the user may
                -- get very unintuitive errors when turning sandboxing on. Thus
                -- we will import the Prelude if the operation fails...
                -- I guess this may lead to even more obscure errors, but
                -- hopefully in much less frequent situations...
                r <- go
                      `catchError` (\err -> case err of
                                             WontCompile _ -> do removeTarget t
                                                                 write_mod False
                                                                 go
                                             _             -> throwError err)
                --
                removeTarget t
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
                                               return () -- removeFile f


addTarget :: GHC.Target -> Interpreter ()
addTarget t = do ghc_session <- fromSessionState ghcSession
                 mayFail $ do GHC.addTarget ghc_session t
                              res <- GHC.load ghc_session GHC.LoadAllTargets
                              return $ guard (isSucceeded res) >> Just ()

removeTarget :: GHC.Target -> Interpreter ()
removeTarget t = do ghc_session <- fromSessionState ghcSession
                    mayFail $ do GHC.removeTarget ghc_session (targetId t)
                                 res <- GHC.load ghc_session GHC.LoadAllTargets
                                 return $ guard (isSucceeded res) >> Just ()

targetId :: GHC.Target -> GHC.TargetId
targetId (GHC.Target _id _) = _id

fileTarget :: FilePath -> GHC.Target
fileTarget f = GHC.Target (GHC.TargetFile f $ Just next_phase) Nothing
    where next_phase = DP.Cpp DP.HsSrcFile

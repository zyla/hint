module Main ( main ) where

import Control.Exception

import Control.Monad       ( liftM, when )
import Control.Monad.Trans ( MonadIO(liftIO) )
import Control.Monad.Error ( catchError )

import System.IO
import System.Directory
import System.Exit


import Test.HUnit ( (@?=), (@?) )
import qualified Test.HUnit as HUnit

import qualified Language.Haskell.Interpreter.GHC as H

test_reload_modified :: H.InterpreterSession -> HUnit.Test
test_reload_modified s = testCase "reload_modified" [mod_file] $ do
                            writeFile mod_file mod_v1
                            f_v1 <- H.withSession s get_f
                            --
                            writeFile mod_file mod_v2
                            f_v2 <- H.withSession s get_f
                            --
                            (f_v1 5, f_v2 5) @?= (5, 6)
    --
    where mod_name = "TEST_ReloadModified"
          mod_file = mod_name ++ ".hs"
          --
          mod_v1   = unlines ["module " ++ mod_name,
                              "where",
                              "f :: Int -> Int",
                              "f = id"]
          mod_v2   = unlines ["module " ++ mod_name,
                              "where",
                              "f :: Int -> Int",
                              "f = (1 +)"]
          --
          get_f    =  do H.loadModules [mod_file]
                         H.setTopLevelModules [mod_name]
                         H.interpret "f" (H.as :: Int -> Int)

test_lang_exts :: H.InterpreterSession -> HUnit.Test
test_lang_exts s = testCase "lang_exts" [mod_file] $ do
                      writeFile mod_file "data T where T :: T"
                      H.withSession s $ do
                        loadFails @@? "first time, it shouldn't load"
                        --
                        H.setUseLanguageExtensions True
                        loadSucceeds @@? "now, it should load"
                        --
                        H.setUseLanguageExtensions False
                        loadFails @@? "it shouldn't load, again"
    --
    where mod_name     = "TEST_LangExts"
          mod_file     = mod_name ++ ".hs"
          --
          loadFails    = not `liftM` loadSucceeds
          loadSucceeds = do H.loadModules [mod_name]
                            return True
                         `catchError` (\_ -> return False)

test_work_in_main :: H.InterpreterSession -> HUnit.Test
test_work_in_main s = testCase "work_in_main" [mod_file] $ do
                        writeFile mod_file "f = id"
                        H.withSession s $ do
                          H.loadModules [mod_file]
                          H.setTopLevelModules ["Main"]
                          H.setImportsQ [("Prelude",Nothing),
                                         ("Data.Maybe", Just "Mb")]
                          --
                          H.typeOf "f $ 1+1" @@?= "(Num a) => a"
                          H.eval "f . Mb.fromJust $ Just [1,2]" @@?= "[1,2]"
                          H.interpret "f $ 1 == 2" H.infer @@?= False
    --
    where mod_file     = "TEST_WorkInMain.hs"

test_priv_syms_in_scope :: H.InterpreterSession -> HUnit.Test
test_priv_syms_in_scope s = testCase "private_syms_in_scope" [mod_file] $ do
                               writeFile mod_file mod_text
                               H.withSession s $ do
                                 H.loadModules [mod_file]
                                 H.setTopLevelModules ["T"]
                                 H.typeChecks "g" @@? "g is hidden"
    where mod_text = unlines ["module T(f) where", "f = g", "g = id"]
          mod_file = "TEST_PrivateSymbolsInScope.hs"

test_comments_in_expr :: H.InterpreterSession -> HUnit.Test
test_comments_in_expr s = testCase "comments_in_expr" [] $ do
                               H.withSession s $ do
                                 H.reset
                                 H.setImports ["Prelude"]
                                 let expr = "length $ concat [[1,2],[3]] -- bla"
                                 H.typeChecks expr @@? "comment on expression"
                                 H.eval expr
                                 H.interpret expr (H.as :: Int)
                                 return ()

test_qual_import :: H.InterpreterSession -> HUnit.Test
test_qual_import s = testCase "qual_import" [] $ do
                         H.withSession s $ do
                           H.reset
                           H.setImportsQ [("Prelude", Nothing),
                                          ("Data.Map", Just "M")]
                           H.typeChecks "null []" @@? "Unqual null"
                           H.typeChecks "M.null M.empty" @@? "Qual null"
                           return ()

common_tests :: H.InterpreterSession -> [HUnit.Test]
common_tests s = [test_reload_modified s,
                  test_lang_exts s,
                  test_work_in_main s,
                  test_comments_in_expr s,
                  test_qual_import s]


non_sb_tests :: H.InterpreterSession -> HUnit.Test
non_sb_tests s = HUnit.TestList $ common_tests s ++ [test_priv_syms_in_scope s]

sb_tests :: H.InterpreterSession -> HUnit.Test
sb_tests s = HUnit.TestList $ common_tests s

main :: IO ()
main = do s <- H.newSession
          --
          -- run the tests...
          c <- HUnit.runTestTT $ non_sb_tests s
          -- then run again, but with sandboxing on...
          setSandbox s
          c' <- HUnit.runTestTT $ sb_tests s
          --
          let failures  = HUnit.errors c  + HUnit.failures c +
                          HUnit.errors c' + HUnit.failures c'
              exit_code
                  | failures > 0 = ExitFailure failures
                  | otherwise    = ExitSuccess
          exitWith exit_code
       `catchDyn` (printInterpreterError >=> \_ -> exitWith (ExitFailure $ -1))

printInterpreterError :: H.InterpreterError -> IO ()
printInterpreterError = hPutStrLn stderr . show

setSandbox :: H.InterpreterSession -> IO ()
setSandbox s = H.withSession s $ H.setInstalledModsAreInScopeQualified False

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \a -> f a >>= g

(@@?) :: (HUnit.AssertionPredicable p, MonadIO m) => m p -> String -> m ()
p @@? msg = do b <- p; liftIO (b @? msg)

(@@?=) :: (Eq a, Show a, MonadIO m) => m a -> a -> m ()
m_a @@?= b = do a <- m_a; liftIO (a @?= b)

testCase :: String -> [FilePath] -> HUnit.Assertion -> HUnit.Test
testCase title tmps test = HUnit.TestLabel title $
                               HUnit.TestCase (test' `finally` clean_up)
    where test' = test `catchDyn` (printInterpreterError >=> throwDyn)
          clean_up = mapM_ removeIfExists tmps
          removeIfExists f = do exists <- doesFileExist f
                                when exists $
                                     removeFile f

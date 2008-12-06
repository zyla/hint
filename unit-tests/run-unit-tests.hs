module Main ( main ) where

import Prelude hiding (catch)

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

test_reload_modified :: TestCase
test_reload_modified = TestCase "reload_modified" [mod_file] $ do
                            liftIO $ writeFile mod_file mod_v1
                            f_v1 <- get_f
                            --
                            liftIO $ writeFile mod_file mod_v2
                            f_v2 <- get_f
                            --
                            liftIO $ (f_v1 5, f_v2 5) @?= (5, 6)
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

test_lang_exts :: TestCase
test_lang_exts = TestCase "lang_exts" [mod_file] $ do
                      liftIO $ writeFile mod_file "data T where T :: T"
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

test_work_in_main :: TestCase
test_work_in_main = TestCase "work_in_main" [mod_file] $ do
                        liftIO $ writeFile mod_file "f = id"
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

test_priv_syms_in_scope :: TestCase
test_priv_syms_in_scope = TestCase "private_syms_in_scope" [mod_file] $ do
                               liftIO $ writeFile mod_file mod_text
                               H.loadModules [mod_file]
                               H.setTopLevelModules ["T"]
                               H.typeChecks "g" @@? "g is hidden"
    where mod_text = unlines ["module T(f) where", "f = g", "g = id"]
          mod_file = "TEST_PrivateSymbolsInScope.hs"

test_comments_in_expr :: TestCase
test_comments_in_expr = TestCase "comments_in_expr" [] $ do
                            H.setImports ["Prelude"]
                            let expr = "length $ concat [[1,2],[3]] -- bla"
                            H.typeChecks expr @@? "comment on expression"
                            H.eval expr
                            H.interpret expr (H.as :: Int)
                            return ()

test_qual_import :: TestCase
test_qual_import = TestCase "qual_import" [] $ do
                           H.setImportsQ [("Prelude", Nothing),
                                          ("Data.Map", Just "M")]
                           H.typeChecks "null []" @@? "Unqual null"
                           H.typeChecks "M.null M.empty" @@? "Qual null"
                           return ()

test_basic_eval :: TestCase
test_basic_eval = TestCase "basic_eval" [] $ do
                           H.eval "()" @@?= "()"

common_tests :: [TestCase]
common_tests = [test_reload_modified,
                test_lang_exts,
                test_work_in_main,
                test_comments_in_expr,
                test_qual_import,
                test_basic_eval]

non_sb_tests :: [TestCase]
non_sb_tests = common_tests ++ [test_priv_syms_in_scope]

sb_tests :: [TestCase]
sb_tests = common_tests

main :: IO ()
main = do -- run the tests...
          c  <- runTests False non_sb_tests
          -- then run again, but with sandboxing on...
          c' <- runTests True sb_tests
          --
          let failures  = HUnit.errors c  + HUnit.failures c +
                          HUnit.errors c' + HUnit.failures c'
              exit_code
                  | failures > 0 = ExitFailure failures
                  | otherwise    = ExitSuccess
          exitWith exit_code
       -- `catch` (\_ -> exitWith (ExitFailure $ -1))

printInterpreterError :: H.InterpreterError -> IO ()
printInterpreterError = hPutStrLn stderr . show

setSandbox :: H.Interpreter ()
setSandbox = H.setInstalledModsAreInScopeQualified False

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \a -> f a >>= g

(@@?) :: (HUnit.AssertionPredicable p, MonadIO m) => m p -> String -> m ()
p @@? msg = do b <- p; liftIO (b @? msg)

(@@?=) :: (Eq a, Show a, MonadIO m) => m a -> a -> m ()
m_a @@?= b = do a <- m_a; liftIO (a @?= b)


data TestCase = TestCase String [FilePath] (H.Interpreter ())

runTests :: Bool -> [TestCase] -> IO HUnit.Counts
runTests sandboxed = HUnit.runTestTT . HUnit.TestList . map build
    where build (TestCase title tmps test) = HUnit.TestLabel title $
                                                 HUnit.TestCase test_case
            where test_case = go `finally` clean_up
                  clean_up = mapM_ removeIfExists tmps
                  go       = do r <- H.runInterpreter
                                            (when sandboxed setSandbox >> test)
                                either (printInterpreterError >=> (fail . show))
                                       return r
                  removeIfExists f = do exists <- doesFileExist f
                                        when exists $
                                          removeFile f

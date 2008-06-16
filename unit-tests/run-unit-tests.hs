import Control.Exception   ( catchDyn, finally )

import Control.Monad       ( liftM, when )
import Control.Monad.Trans ( MonadIO(liftIO) )
import Control.Monad.Error ( catchError )

import System.Directory    ( doesFileExist, removeFile )
import System.Exit

import Test.HUnit ( (@?=), (@?) )
import qualified Test.HUnit as HUnit

import qualified Language.Haskell.Interpreter.GHC as H

test_reload_modified :: H.InterpreterSession -> HUnit.Test
test_reload_modified s = testCase "reload_modifie" [mod_file] $ do
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

main :: IO ()
main = do s <- H.newSession
          --
          c <- HUnit.runTestTT $ HUnit.TestList [
                   test_reload_modified s,
                   test_lang_exts s]
          --
          let failures  = HUnit.errors c + HUnit.failures c
              exit_code
                  | failures > 0 = ExitFailure failures
                  | otherwise    = ExitSuccess
          exitWith exit_code
       `catchDyn` printInterpreterError

printInterpreterError :: H.InterpreterError -> IO ()
printInterpreterError e = do putStrLn $ "Ups... " ++ (show e)
                             exitWith (ExitFailure $ -1)

(@@?) :: (HUnit.AssertionPredicable p, MonadIO m) => m p -> String -> m ()
p @@? msg = do b <- p; liftIO (b @? msg)

testCase :: String -> [FilePath] -> HUnit.Assertion -> HUnit.Test
testCase title tmps test = HUnit.TestLabel title $
                               HUnit.TestCase (test `finally` clean_up)
    where clean_up = mapM_ removeIfExists tmps
          removeIfExists f = do exists <- doesFileExist f
                                when exists $
                                     removeFile f
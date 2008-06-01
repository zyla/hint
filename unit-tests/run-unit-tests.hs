import Control.Exception   ( catchDyn )
import System.Directory    ( removeFile )

import qualified Language.Haskell.Interpreter.GHC as H

test_reload_modified :: H.InterpreterSession -> IO Bool
test_reload_modified s = do writeFile mod_file mod_v1
                            f_v1 <- H.withSession s get_f
                            --
                            writeFile mod_file mod_v2
                            f_v2 <- H.withSession s get_f
                            --
                            removeFile mod_file
                            --
                            return $ (f_v1 5, f_v2 5) == (5, 6)
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

main :: IO ()
main = do s <- H.newSession
          ok <- test_reload_modified s
          if ok
            then putStrLn "test ok"
            else fail "test failed"
       `catchDyn` printInterpreterError

printInterpreterError :: H.InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Ups... " ++ (show e)

{-# OPTIONS_GHC -fglasgow-exts #-}

import Control.Monad
import Control.Monad.Trans
import Language.Haskell.Interpreter.GHC

main = testHint
                  
testHint =
    do
        s <- newSessionUsing "/opt/local/lib/ghc-6.6"
        r <- withSession s $ do
            loadModules ["t.hs"]
            setTopLevelModules ["Main"]
            setImports ["Prelude"]
            a <- eval "length $ unwords [\"hello\", \"world\"]"
            liftIO $ print a
            b <- interpret "head $ map show [True,False]" infer >>= flip interpret (as :: Bool)
            liftIO $ if b then print "b has type Bool" else print "won't happen"
            liftIO . print =<< typeOf "(f,g)"
            return "thats'all folks"
        print r

f = unwords ["\\x -> let join = Control.Monad.join;",
             "           interpret = Language.Haskell.Interpreter.GHC.interpret;",
             "           infer = Language.Haskell.Interpreter.GHC.infer;",
             "           as = Language.Haskell.Interpreter.GHC.infer",
             "       in (interpret x infer >>= flip interpret (as :: Language.Haskell.Interpreter.GHC.Interpreter String))"]

g = interpret f (as :: Interpreter String)


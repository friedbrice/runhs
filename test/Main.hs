module Main (main) where

import Test.Hspec
import qualified System.Directory as Sys
import qualified System.Exit as Sys
import qualified System.Process as Sys

data Mode = Watch | Repl | Script | Compile

render Watch = "watch"
render Repl = "repl"
render Script = "script"
render Compile = "compile"

type Stdin = String
type Stderr = String
type Stdout = String

runhs :: Mode -> FilePath -> [String] -> Stdin -> IO (Bool, Stdout, Stderr)
runhs mode path args stdin =
    isSuccess <$> Sys.readProcessWithExitCode "stack" args' stdin
    where
    args' = ["exec", "--resolver", "nightly", "runhs", render mode, path] <> args
    isSuccess (ec, so, se) = (ec == Sys.ExitSuccess, so, se)

main :: IO ()
main = hspec $ do
    describe "hello-haskell test" $ do
        let helloHaskell = "test/resources/hello-haskell.hs"

        it "should load in repl mode" $ do
            (success, _, _) <- runhs Repl helloHaskell [] ":t greet\n:q"
            success `shouldBe` True

        it "should load in watch mode" $ do
            _ <- runhs Watch helloHaskell ["--allow-eval"] ""
            return ()

        it "should load in script mode" $ do
            (success, out, _) <- runhs Script helloHaskell [] ""
            success `shouldBe` True
            out `shouldBe` "Hello, World!\n"

        it "should forward arguments in script mode" $ do
            (success, out, _) <- runhs Script helloHaskell ["Veni", "Vidi"] ""
            success `shouldBe` True
            out `shouldBe` "Hello, Veni!\nHello, Vidi!\n"

        it "should forward stdin in script mode" $ do
            (success, out, _) <- runhs Script helloHaskell [] "Veni Vidi\nVici"
            success `shouldBe` True
            out `shouldBe` "Hello, Veni!\nHello, Vidi!\nHello, Vici!\n"

        it "should load in compile mode" $ do
            (success, out, _) <- runhs Compile helloHaskell [] ""
            success `shouldBe` True
            Sys.removePathForcibly "test/resources/hello-haskell.o"
            Sys.removePathForcibly "test/resources/hello-haskell.hi"
            Sys.removePathForcibly "test/resources/hello-haskell"

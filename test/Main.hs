{-
resolver: nightly
packages:
  - directory
  - hspec
  - process
-}
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
type Stdout = String
type Stderr = String

runhs :: Mode -> FilePath -> [String] -> Stdin -> IO (Sys.ExitCode, Stdout, Stderr)
runhs mode path args stdin =
    Sys.readProcessWithExitCode "stack" args' stdin
    where
    args' = ["exec", "--resolver", "nightly", "runhs", render mode, path] <> args

main :: IO ()
main = hspec $ do
    describe "hello-haskell test" $ do
        let helloHaskell = "test/resources/hello-haskell.hs"

        it "should load in repl mode" $ do
            (status, _, _) <- runhs Repl helloHaskell [] ":t greet\n:q"
            status `shouldBe` Sys.ExitSuccess

        it "should load in watch mode" $ do
            _ <- runhs Watch helloHaskell ["--allow-eval"] ""
            -- Ghcid exits with success on Windows, with error on Unix.
            return ()

        it "should load in script mode" $ do
            (status, out, _) <- runhs Script helloHaskell [] ""
            status `shouldBe` Sys.ExitSuccess
            out `shouldBe` "Hello, World!\n"

        it "should forward arguments in script mode" $ do
            (status, out, _) <- runhs Script helloHaskell ["Veni", "Vidi"] ""
            status `shouldBe` Sys.ExitSuccess
            out `shouldBe` "Hello, Veni!\nHello, Vidi!\n"

        it "should forward stdin in script mode" $ do
            (status, out, _) <- runhs Script helloHaskell [] "Veni Vidi\nVici"
            status `shouldBe` Sys.ExitSuccess
            out `shouldBe` "Hello, Veni!\nHello, Vidi!\nHello, Vici!\n"

        it "should load in compile mode" $ do
            (status, out, _) <- runhs Compile helloHaskell [] ""
            status `shouldBe` Sys.ExitSuccess
            Sys.removePathForcibly "test/resources/hello-haskell.o"
            Sys.removePathForcibly "test/resources/hello-haskell.hi"
            Sys.removePathForcibly "test/resources/hello-haskell"

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
    args' = ["exec", "--resolver", "nightly", "runhs", "--", render mode, path] <> args

main :: IO ()
main = hspec $ do
    describe "hello-haskell test" $ do
        let helloHaskell = "test/resources/hello-haskell.hs"

        it "should load in repl mode" $ do
            (status, out, err) <- runhs Repl helloHaskell [] ":t greet\n:q"
            status `shouldBe` Sys.ExitSuccess
            out `shouldContain` "greet :: [String] -> [IO ()]"
            err `shouldContain` "Selected resolver: nightly"

        it "should load in watch mode" $ do
            (_, out, err) <- runhs Watch helloHaskell ["--allow-eval"] ""
            -- Ghcid exits with success on Windows, with error on Unix.
            out `shouldContain` "exited unexpectedly"
            err `shouldContain` "Selected resolver: nightly"

        it "should load in script mode" $ do
            (status, out, err) <- runhs Script helloHaskell [] ""
            status `shouldBe` Sys.ExitSuccess
            out `shouldBe` "Hello, World!\n"
            err `shouldContain` "Selected resolver: nightly"

        it "should forward arguments in script mode" $ do
            (status, out, err) <- runhs Script helloHaskell ["Veni", "Vidi"] ""
            status `shouldBe` Sys.ExitSuccess
            out `shouldBe` "Hello, Veni!\nHello, Vidi!\n"
            err `shouldContain` "Selected resolver: nightly"

        it "should forward stdin in script mode" $ do
            (status, out, err) <- runhs Script helloHaskell [] "Veni Vidi\nVici"
            status `shouldBe` Sys.ExitSuccess
            out `shouldBe` "Hello, Veni!\nHello, Vidi!\nHello, Vici!\n"
            err `shouldContain` "Selected resolver: nightly"

        it "should load in compile mode" $ do
            (status, out, err) <- runhs Compile helloHaskell [] ""
            status `shouldBe` Sys.ExitSuccess
            out `shouldContain` "Linking test/resources/hello-haskell"
            err `shouldContain` "Selected resolver: nightly"
            Sys.removePathForcibly "test/resources/hello-haskell.o"
            Sys.removePathForcibly "test/resources/hello-haskell.hi"
            Sys.removePathForcibly "test/resources/hello-haskell"

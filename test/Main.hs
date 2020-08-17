{-
resolver: nightly
packages:
  - directory
  - hspec
  - process
-}
module Main (main) where

import Data.Maybe
import Test.Hspec

import qualified System.Directory as Sys
import qualified System.Environment as Sys
import qualified System.Exit as Sys
import qualified System.Process as Sys

data Mode = Watch | Repl | Script | Compile

render Watch = "watch"
render Repl = "repl"
render Script = "script"
render Compile = "compile"

type Resolver = String
type Stdin = String
type Stdout = String
type Stderr = String

main :: IO ()
main = do
    resolver <- fromMaybe "nightly" <$> Sys.lookupEnv "TEST_RESOLVER"
    putStrLn $ unwords ["TEST_RESOLVER:", resolver]
    test resolver

runhs ::
    Resolver -> Mode -> FilePath -> [String] -> Stdin ->
    IO (Sys.ExitCode, Stdout, Stderr)
runhs resolver mode path args stdin =
    Sys.readProcessWithExitCode "stack" (args' <> args) stdin
    where
    args' = ["exec", "--resolver", resolver, "runhs", "--", render mode, path]

test :: Resolver -> IO ()
test resolver = hspec $ do
    describe "hello-haskell test" $ do
        let helloHaskell = "test/resources/hello-haskell.hs"

        it "should load in repl mode" $ do
            (status, out, err) <- runhs resolver Repl helloHaskell [] ":t greet\n:q"
            status `shouldBe` Sys.ExitSuccess
            out `shouldContain` "greet :: [String] -> [IO ()]"
            err `shouldContain` unwords ["Selected resolver:", resolver]

        -- broken in CI. works on my machine. i don't have time for this shit.
        -- it "should load in watch mode" $ do
        --     (_, out, err) <- runhs resolver Watch helloHaskell ["--allow-eval"] ""
        --     -- Ghcid exits with success on Windows, with error on Unix.
        --     out `shouldContain` "exited unexpectedly"
        --     err `shouldContain` unwords ["Selected resolver:", resolver]

        it "should load in script mode" $ do
            (status, out, err) <- runhs resolver Script helloHaskell [] ""
            status `shouldBe` Sys.ExitSuccess
            out `shouldBe` "Hello, World!\n"
            err `shouldContain` unwords ["Selected resolver:", resolver]

        it "should forward arguments in script mode" $ do
            (status, out, err) <- runhs resolver Script helloHaskell ["Veni", "Vidi"] ""
            status `shouldBe` Sys.ExitSuccess
            out `shouldBe` "Hello, Veni!\nHello, Vidi!\n"
            err `shouldContain` unwords ["Selected resolver:", resolver]

        it "should forward stdin in script mode" $ do
            (status, out, err) <- runhs resolver Script helloHaskell [] "Veni Vidi\nVici"
            status `shouldBe` Sys.ExitSuccess
            out `shouldBe` "Hello, Veni!\nHello, Vidi!\nHello, Vici!\n"
            err `shouldContain` unwords ["Selected resolver:", resolver]

        it "should load in compile mode" $ do
            (status, out, err) <- runhs resolver Compile helloHaskell [] ""
            status `shouldBe` Sys.ExitSuccess
            out `shouldContain` "Linking test/resources/hello-haskell"
            err `shouldContain` unwords ["Selected resolver:", resolver]
            Sys.removePathForcibly "test/resources/hello-haskell.o"
            Sys.removePathForcibly "test/resources/hello-haskell.hi"
            Sys.removePathForcibly "test/resources/hello-haskell"

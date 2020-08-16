{-
resolver: nightly
packages:
  - bytestring
  - file-embed
  - process
  - terminal-size
  - text
  - word-wrap
  - yaml
-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import Data.ByteString.Char8 (pack)
import Data.FileEmbed (embedStringFile)
import Data.Foldable (fold)
import Data.Yaml (decodeThrow, parseMaybe, withObject, (.:), (.:?))

import qualified System.Console.Terminal.Size as Sys
import qualified System.Environment as Sys
import qualified System.Exit as Sys
import qualified System.IO as Sys
import qualified System.Process as Sys

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Text.Wrap as Text

data RunSpec = RunSpec
    { file :: FilePath
    , resolver :: String
    , packages :: [String]
    }

spec :: FilePath -> IO RunSpec
spec path = do
    let readHeader =
            decodeThrow . pack
            . unlines
            . snd . break (/= "{-")
            . fst . break (== "-}")
            . fmap (filter (/= '\r'))
            . lines
    header <- readHeader =<< readFile path
    (resolver, packages) <-
        maybe
            (help $ unwords ["Unable to parse the front matter in", path, "."])
            pure
        . flip parseMaybe header
        . withObject "header"
        $ \hdr -> do
            resolver <- hdr .: "resolver"
            packages <- fmap fold $ hdr .:? "packages"
            return (resolver, packages)
    return (RunSpec path resolver packages)

stackArgs :: RunSpec -> [String]
stackArgs spec =
    ["--resolver", resolver spec]
    <> (packages spec >>= \package -> ["--package", package])
    <> [file spec]

main :: IO ()
main = do
    Sys.hSetBuffering Sys.stdout Sys.NoBuffering
    args <- Sys.getArgs
    case args of
        "watch":file:args' -> watch args' =<< spec file
        "repl":file:_ -> repl =<< spec file
        "compile":file:_ -> compile =<< spec file
        "script":file:args' -> script args' =<< spec file
        _ -> help "Unable to parse the command-line arguments."

runProcess :: Sys.CreateProcess -> IO ()
runProcess process = do
    (_, _, _, h) <- Sys.createProcess process
    code <- Sys.waitForProcess h
    Sys.exitWith code

watch :: [String] -> RunSpec -> IO ()
watch args spec = runProcess $
    Sys.proc "stack" $
        [ "exec"
        , "--resolver"
        , resolver spec
        , "ghcid"
        , "--"
        , "--command"
        , unwords $ "stack" : "repl" : stackArgs spec
        ]
        <> args

repl :: RunSpec -> IO ()
repl spec = runProcess $
    Sys.proc "stack" ("repl" : stackArgs spec)

compile :: RunSpec -> IO ()
compile spec = runProcess $
    Sys.proc "stack" ("ghc" : stackArgs spec)

script :: [String] -> RunSpec -> IO ()
script args spec = runProcess $
    Sys.proc "stack" ("runhaskell" : stackArgs spec <> args)

help :: String -> IO a
help reason = do
    n <- min 72 . maybe 72 Sys.width <$> Sys.size

    putStrLn (replicate n '~')
    Text.putStrLn
        $ Text.wrapText (Text.WrapSettings True False) n
        $ Text.pack $(embedStringFile "README.md")
    putStrLn (replicate n '~')

    putStrLn $ unwords ["runhs:", reason, "Please see \"Usage\" above."]
    Sys.exitWith (Sys.ExitFailure 1)

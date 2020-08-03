{-
resolver: nightly
packages:
  - bytestring
  - file-embed
  - process
  - yaml
-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import Data.ByteString.Char8 (pack)
import Data.FileEmbed (embedStringFile)
import Data.Foldable (fold)
import Data.Yaml (decodeThrow, parseMaybe, withObject, (.:), (.:?))
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import System.Process (CreateProcess, createProcess, proc, waitForProcess)

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
            (help $ "unable to parse the front matter in " <> path)
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
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case args of
        "watch":file:args' -> watch args' =<< spec file
        "repl":file:_ -> repl =<< spec file
        "compile":file:_ -> compile =<< spec file
        "script":file:args' -> script args' =<< spec file
        _ -> help "unable to parse the command-line arguments"

runProcess :: CreateProcess -> IO ()
runProcess process = do
    (_, _, _, h) <- createProcess process
    code <- waitForProcess h
    exitWith code

watch :: [String] -> RunSpec -> IO ()
watch args spec = runProcess $
    proc "stack" $
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
    proc "stack" ("repl" : stackArgs spec)

compile :: RunSpec -> IO ()
compile spec = runProcess $
    proc "stack" ("ghc" : stackArgs spec)

script :: [String] -> RunSpec -> IO ()
script args spec = runProcess $
    proc "stack" ("runhaskell" : stackArgs spec <> args)

help :: String -> IO a
help reason = do
    putStrLn (replicate 40 '~')
    putStrLn $(embedStringFile "README.md")
    putStrLn (replicate 40 '~')
    putStrLn $ "runhs: " <> reason <> ". Please see ``Usage'' above."
    exitWith (ExitFailure 1)

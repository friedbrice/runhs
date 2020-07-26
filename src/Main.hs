{-
resolver: nightly
packages:
  - bytestring
  - process
  - yaml
-}
{-# LANGUAGE ImplicitParams, OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import Control.Monad (void)
import Data.ByteString.Char8 (pack)
import Data.FileEmbed (embedStringFile)
import Data.Foldable (fold, toList)
import Data.Yaml (Value, decodeThrow, parseMaybe, withObject, (.:), (.:?))
import GHC.Exception (CallStack)
import System.Environment (getArgs)
import System.Exit (exitWith)
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
        maybe (help "spec") pure
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
        "watch":file:_ -> watch =<< spec file
        "repl":file:_ -> repl =<< spec file
        "compile":file:_ -> compile =<< spec file
        "run":file:args' -> run args' =<< spec file
        _ -> help "main"

go :: CreateProcess -> IO ()
go process = do
    (_, _, _, h) <- createProcess process
    code <- waitForProcess h
    exitWith code

watch :: RunSpec -> IO ()
watch spec = go $
    proc "stack"
        [ "exec"
        , "--resolver"
        , resolver spec
        , "ghcid"
        , "--"
        , "--command"
        , unwords $ "stack" : "repl" : stackArgs spec
        ]

repl :: RunSpec -> IO ()
repl spec = go $
    proc "stack" ("repl" : stackArgs spec)

compile :: RunSpec -> IO ()
compile spec = go $
    proc "stack" ("ghc" : stackArgs spec)

run :: [String] -> RunSpec -> IO ()
run args spec = go $
    proc "stack" ("runhaskell" : stackArgs spec <> args)

help :: (?loc :: CallStack) => String -> IO a
help trace = do
    putStrLn $(embedStringFile "README.md")
    fail (trace <> ": " <> show ?loc)

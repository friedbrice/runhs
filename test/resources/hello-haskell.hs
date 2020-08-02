{-
resolver: lts-16.8
packages:
  - text
-}
{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  args <- getArgs
  stdin <- T.getContents
  let names = args <> map T.unpack (concatMap T.words (T.lines stdin))
  sequence_ (greet names)

greet :: [String] -> [IO ()]
greet [] = [T.putStrLn "Hello, World!"]
greet xs = fmap (\name -> T.putStrLn ("Hello, " <> T.pack name <> "!")) xs

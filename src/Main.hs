module Main where

import System.Environment

import qualified Day1 as Day1
import qualified Day2 as Day2

run :: String -> [String] -> String
run "1.1" = Day1.run
run "1.2" = Day1.run2
run "2.1" = Day2.run
run "2.2" = Day2.run2

main :: IO ()
main = do
  args <- getArgs
  let program = head args
  input <- lines <$> getContents
  let output = run program input
  putStrLn output

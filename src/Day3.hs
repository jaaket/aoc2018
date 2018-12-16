module Day3 where

import Data.Hashable
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.Monoid
import Control.Monad
import Data.Either (fromRight)
import Text.Parsec
import Text.Parsec.String


data Claim = Claim
  { id :: Int
  , left :: Int
  , top :: Int
  , width :: Int
  , height :: Int
} deriving Show

int :: Parser Int
int = read <$> many1 digit

parser :: Parser Claim
parser = do
  char '#'
  id <- int
  spaces
  char '@'
  spaces
  left <- int
  char ','
  top <- int
  char ':'
  spaces
  width <- int
  char 'x'
  height <- int
  return (Claim id left top width height)

readLine :: String -> Claim
readLine str = result
  where Right result = parse parser "" str

run :: [String] -> String
run = undefined
module Day1 where

import Data.Hashable
import qualified Data.HashSet as Set
import Control.Monad
import Data.Monoid
import Data.Either (fromRight)
import Text.Parsec
import Text.Parsec.String

parser :: Parser Int
parser = do
  sign <- char '+' <|> char '-'
  num <- read <$> many1 digit
  if sign == '+'
    then return num
    else return (-num)

readLine :: String -> Int
readLine str = result
  where Right result = parse parser "" str

run :: [String] -> String
run = show . foldl1 (+) . map readLine

findDuplicate :: (Eq a, Hashable a) => [a] -> Maybe a
findDuplicate values =
  let action seenValues value =
        if Set.member value seenValues
          then Left value
          else Right (Set.insert value seenValues)
  in case foldM action Set.empty values of
    Left found -> Just found
    Right _ -> Nothing

run2 :: [String] -> String
run2 = show . findDuplicate . scanl1 (+) . join . repeat . map readLine

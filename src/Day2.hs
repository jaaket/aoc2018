module Day2 where

import Data.Hashable
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.Monoid
import Control.Monad
import Data.Either (fromRight)
import Text.Parsec
import Text.Parsec.String

countLetters :: String -> Map.HashMap Char Int
countLetters = foldl (\acc c -> Map.insertWith (+) c 1 acc) Map.empty

run :: [String] -> String
run ss =
  let counts = map countLetters ss
      exactlyTwo = length (filter (elem 2 . Map.elems) counts)
      exactlyThree = length (filter (elem 3 . Map.elems) counts)
  in show (exactlyTwo * exactlyThree)

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

countDiffering :: String -> String -> Int
countDiffering = length . filter id .: zipWith (/=)

run2 :: [String] -> String
run2 ss = show [map fst $ filter (uncurry (==)) (zip s1 s2) | s1 <- ss, s2 <- ss, countDiffering s1 s2 == 1]
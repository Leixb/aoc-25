module Main where

import Control.Monad
import Data.Char
import Data.List qualified as L
import Data.Map
import Data.Set
import Data.Set qualified as S
import Text.ParserCombinators.ReadP

parseInt :: ReadP Int
parseInt = read <$> munch1 isDigit

parse = (,) <$> (fromListWith S.union <$> parseOrder) <*> (char '\n' *> parseUpdate)
parseOrder = endBy (flip (,) <$> (S.singleton <$> parseInt <* char '|') <*> parseInt) (char '\n')
parseUpdate = endBy (sepBy parseInt (char ',')) (char '\n')

verify :: Map Int (Set Int) -> [Int] -> Bool
verify m = and . ap (zipWith fn) (scanl (flip S.insert) S.empty)
  where
    fn a = flip S.isSubsetOf (findWithDefault S.empty a m)

getMiddle = ap (!!) (flip div 2 . length)

part1 m = sum . fmap getMiddle . L.filter (verify m)

main = getContents >>= print . uncurry part1 . fst . last . readP_to_S parse

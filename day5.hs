module Main where

import Control.Arrow
import Data.Char
import Data.List qualified as L
import Data.Map
import Data.Set
import Data.Set qualified as S
import Text.ParserCombinators.ReadP

parse = (,) <$> (fromListWith S.union <$> parseOrder) <*> (eol *> parseUpdate)
parseOrder = endBy (flip (,) <$> (S.singleton <$> parseInt <* char '|') <*> parseInt) eol
parseUpdate = endBy (sepBy parseInt (char ',')) eol
parseInt = read <$> munch1 isDigit
eol = char '\n'

verify :: Map Int (Set Int) -> [Int] -> Bool
verify m = and . (zipWith fn <*> scanl (flip S.insert) S.empty)
  where
    fn a = flip S.isSubsetOf (findWithDefault S.empty a m)

getMiddle = (!!) <*> ((`div` 2) . length)

part1 m = sum . fmap getMiddle . L.filter (verify m)

getOrigin :: Map Int (Set Int) -> Set Int -> Int
getOrigin m l = head $ L.filter (S.disjoint l . preds) (S.toList l)
  where
    preds = flip (findWithDefault S.empty) m

order :: Map Int (Set Int) -> Set Int -> [Int]
order m s
  | S.null s = []
  | otherwise = h : order m (S.delete h s)
    where
      h = getOrigin m s

part2 m = sum . fmap (getMiddle . order m . S.fromList) . L.filter (not . verify m)

main = getContents >>= print . (uncurry part1 &&& uncurry part2) . fst . last . readP_to_S parse

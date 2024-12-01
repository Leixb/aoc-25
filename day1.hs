module Main where

import Control.Arrow
import Data.List
import Data.Map

parse = transpose . fmap (fmap read . words) . lines <$> getContents

part1, part2 :: [[Int]] -> Int
part1 [a, b] = sum $ abs <$> zipWith (-) (sort a) (sort b)
part1 _ = undefined

part2 [a, b] = sum $ zipWith (*) a $ fmap (flip (findWithDefault 0) b') a
  where
    b' = fromListWith (+) $ fmap (,1) b
part2 _ = undefined

main = parse >>= print . (part1 &&& part2)

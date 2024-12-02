module Main where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Map

part1 [a, b] = sum $ abs <$> zipWith (-) (sort a) (sort b)
part2 [a, b] = sum $ ap (zipWith (*)) (fmap (flip (findWithDefault 0) (freq b))) a
  where
    freq = fromListWith (+) . fmap (,1)

main = getContents >>= (print . (part1 &&& part2)) . transpose . fmap (fmap read . words) . lines

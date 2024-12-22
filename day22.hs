module Main where

import Control.Arrow
import Data.Bits
import Data.List
import Data.Map qualified as M

parse = fmap (secretNums . read) . lines

secretNums :: Int -> [Int]
secretNums = take 2001 . iterate (step1 >>> step2 >>> step3)
 where
  step1 n = ((n `shiftL` 06) `xor` n) .&. 0xFFFFFF
  step2 n = ((n `shiftR` 05) `xor` n) .&. 0xFFFFFF
  step3 n = ((n `shiftL` 11) `xor` n) .&. 0xFFFFFF

part1 = sum . fmap last
part2 = maximum . M.elems . M.unionsWith (+) . fmap (deltas . fmap (`mod` 10))

deltas l = M.fromListWith (\_ x -> x) $ zip (zip4 diffs (tail diffs) (drop 2 diffs) (drop 3 diffs)) (drop 4 l)
 where
  diffs = zipWith (-) (tail l) l

main = getContents >>= print . (part1 &&& part2) . parse

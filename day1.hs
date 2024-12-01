module Main where

import Data.List
import Control.Arrow
import Data.Map
import qualified Data.Map as M

parse :: IO [[Int]]
parse = transpose . fmap (fmap read . words) . lines <$> getContents

part1 :: [[Int]] -> Int
part1 [a, b] = sum $ abs <$> zipWith (-) (sort a) (sort b)
part1 _ = undefined

part2 :: [[Int]] -> Int
part2 [a, b] = sum a'
    where
        a' = zipWith (*) a $ fmap (flip (M.findWithDefault 0) b') a
        b' = fromListWith (+) $ fmap (,1) b
part2 _ = undefined

main :: IO ()
main = parse >>= print . (part1 &&& part2)

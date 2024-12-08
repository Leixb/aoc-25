module Main where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Map qualified as M

type Pos = [Int]

parse :: String -> (Pos, [(Char, Pos)])
parse s = ([n, m], [(c, [i, j]) | i <- [0 .. n], j <- [0 .. m], c <- [l !! i !! j], c /= '.'])
  where
    l = lines s
    n = pred $ length $ head l
    m = pred $ length l

buildMap :: [(Char, Pos)] -> M.Map Char [Pos]
buildMap = M.fromListWith (++) . fmap (second pure)

allPairs :: [Pos] -> [(Pos, Pos)]
allPairs l = [(x, y) | (x : xs) <- tails l, y <- xs]

add = zipWith (+)
sub = zipWith (-)

antinodes :: Pos -> Pos -> [Pos]
antinodes a b = [a `sub` ab, b `add` ab]
  where
    ab = b `sub` a

inBounds [x', y'] [x, y] = x >= 0 && y >= 0 && x <= x' && y <= y'

antinodes' :: Pos -> Pos -> Pos -> [Pos]
antinodes' l a b = al ++ bl
  where
    ab = b `sub` a
    al = takeWhile (inBounds l) $ iterate (`sub` ab) a
    bl = takeWhile (inBounds l) $ iterate (`add` ab) b

part1 l = length . nub . filter (inBounds l) . concat . M.elems . fmap (allPairs >=> uncurry antinodes)
part2 l = length . nub . concat . M.elems . fmap (allPairs >=> uncurry (antinodes' l))

main = getContents >>= print . (uncurry part1 &&& uncurry part2) . second buildMap . parse

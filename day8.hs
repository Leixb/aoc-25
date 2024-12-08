module Main where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Map qualified as M

type Pos = [Int]

parse :: String -> ([(Char, Pos)], Pos)
parse s = ([(c, [i, j]) | i <- [0 .. n], j <- [0 .. m], c <- [l !! i !! j], c /= '.'], [n, m])
  where
    l = lines s
    n = pred $ length $ head l
    m = pred $ length l

getMap :: [(Char, Pos)] -> M.Map Char [Pos]
getMap = M.fromListWith (++) . fmap (second pure)

allPairs l = [antinodes x y | (x : xs) <- tails l, y <- xs]

add = zipWith (+)
sub = zipWith (-)

antinodes :: Pos -> Pos -> [Pos]
antinodes a b = [a `sub` ab, b `add` ab]
    where
        ab = b `sub` a


inBounds [x', y'] [x, y]  = x >= 0 && y >= 0 && x <= x' && y <= y'

antinodesL :: Pos -> Pos -> Pos -> [Pos]
antinodesL l a b = al ++ bl
    where
        ab = b `sub` a
        al = takeWhile (inBounds l) $ iterate (`sub` ab) a
        bl = takeWhile (inBounds l) $ iterate (`add` ab) b

allPairs' l v = [antinodesL l x y | (x : xs) <- tails v, y <- xs]

part1 m l = length . nub . filter (inBounds l) . concat . concat . M.elems . fmap allPairs . getMap $ m
part2 m l = length . nub . concat . concat . M.elems . fmap (allPairs' l) . getMap $ m

main = getContents >>= print . uncurry part2  . parse

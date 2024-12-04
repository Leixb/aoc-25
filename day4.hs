module Main where

import Control.Arrow
import Data.Array
import Data.List

parse :: String -> Board
parse s = listArray ((1, 1), (n, m)) [l !! i !! j | i <- [0 .. n - 1], j <- [0 .. m - 1]]
  where
    l = lines s
    n = length $ head l
    m = length l

data Dir = N | NE | E | SE | S | SW | W | NW

target = "XMAS"

move N = first pred
move S = first succ
move E = second pred
move W = second succ
move NW = move N . move W
move SW = move S . move W
move NE = move N . move E
move SE = move S . move E

type Pos = (Int, Int)
type Board = Array (Int, Int) Char

inBounds ((al, bl), (a, b)) (a', b') = (a' >= al) && (b' >= bl) && (a' <= a) && (b' <= b)

check :: Board -> Pos -> Int -> Dir -> Bool
check b p i d
    | i >= length target = True
    | not ok = False
    | (b ! p) /= (target !! i) = False
    | otherwise = check b next (succ i) d
  where
    ok = inBounds (bounds b) p
    next = move d p

checkAll :: Board -> Pos -> Int
checkAll b p = length . filter (check b p 0) $ [N, NE, E, SE, S, SW, W, NW]

checkAll2 b p = if check2 b p 0 then 1 else 0

solve b = checkAll b <$> indices b
solve2 b = checkAll2 b <$> indices b

check2 :: Board -> Pos -> Int -> Bool
check2 b p i
    | i >= length target = True
    | not ok = False
    | (b ! p) /= 'A' = False
    | not (
        ((b ! nw) `elem` "SM") ||
        ((b ! ne) `elem` "SM") ||
        ((b ! sw) `elem` "SM") ||
        ((b ! se) `elem` "SM")) = False
    | otherwise = valid
  where
    ok = all (inBounds (bounds b)) [nw, se, sw, nw]
    ne = move NE p
    se = move SE p
    sw = move SW p
    nw = move NW p
    letters = rots $ fmap (b!) [ne, nw, sw, se]
    valid = "SSMM" `elem` letters

rots xs = init (zipWith (++) (tails xs) (inits xs))

main = getContents >>= print . sum . solve2 . parse

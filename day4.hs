module Main where

import Control.Arrow
import Data.Array.Unboxed
import Data.List

type Pos = (Int, Int)
type Board = Array Pos Char
data Dir = N | NE | E | SE | S | SW | W | NW

target = "XMAS"

parse s = listArray ((1, 1), (n, m)) [l !! i !! j | i <- [0 .. n - 1], j <- [0 .. m - 1]]
  where
    l = lines s
    (n, m) = (length $ head l, length l)

move N = first pred
move S = first succ
move E = second pred
move W = second succ
move NW = move N . move W
move SW = move S . move W
move NE = move N . move E
move SE = move S . move E

check :: Board -> Pos -> Int -> Dir -> Bool
check b p i d =
    i >= length target
        || ( inRange (bounds b) p
                && (b ! p) == (target !! i)
                && check b (move d p) (succ i) d
           )

checkAllDirs :: Board -> Pos -> Int
checkAllDirs b p = length . filter (check b p 0) $ [N, NE, E, SE, S, SW, W, NW]

check2 :: Board -> Pos -> Bool
check2 b p =
    all (inRange (bounds b)) moves && ((b ! p) == 'A') && ("SSMM" `elem` rotations)
  where
    rotations = rots $ (b !) <$> moves
    moves = flip move p <$> [NE, SE, SW, NW]

    rots xs = init $ zipWith (++) (tails xs) (inits xs)

part1 b = sum $ checkAllDirs b <$> indices b
part2 b = length . filter (check2 b) $ indices b

main = getContents >>= print . (part1 &&& part2) . parse

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

part1 b = sum $ checkAll b <$> indices b
part2 b = length . filter (flip (check2 b) 0) $ indices b

check2 :: Board -> Pos -> Int -> Bool
check2 b p i
    | i >= length target = True
    | not ok = False
    | (b ! p) /= 'A' = False
    | not (all (`elem` "SM") (head letters)) = False
    | otherwise = "SSMM" `elem` letters
  where
    ok = all (inBounds (bounds b)) moves
    moves = flip move p <$> [NE, SE, SW, NW]
    letters = rots $ fmap (b!) moves

rots xs = init $ zipWith (++) (tails xs) (inits xs)

main = getContents >>= print . (part1 &&& part2) . parse

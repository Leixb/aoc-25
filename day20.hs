module Main where

import Control.Arrow
import Control.Monad
import Data.Array.Unboxed
import Data.Functor
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S

type Pos = (Int, Int)
type Board = Array Pos Char
type Path = M.Map Pos Int

parse board = listArray ((1, 1), (length l, length $ head l)) (concat l)
  where
    l = lines board

moves :: Pos -> [Pos]
moves p = [first succ p, first pred p, second succ p, second pred p]

getOrigin :: Board -> Maybe Pos
getOrigin = fmap fst . find ((== 'S') . snd) . assocs

getPath :: Board -> Pos -> [Pos]
getPath board p
    | not $ inRange (bounds board) p = []
    | board ! p == 'E' = [p]
    | board ! p == '#' = []
    | otherwise = p : (moves p >>= getPath (board // [(p, '#')]))

taxiCab (xa, ya) (xb, yb) = abs (xa - xb) + abs (ya - yb)

solve dist board = do
    path <- M.fromList . flip zip [1 ..] <$> (getOrigin board <&> getPath board)
    let positions = M.keys path
        jumps = [ (path M.! a) - (path M.! b) - d | a <- positions, b <- positions, d <- [taxiCab a b], d <= dist]
    return $ length $ filter (>=100) jumps

main = getContents >>= print . (solve 2 &&& solve 20) . parse

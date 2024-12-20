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

part1 board = do
    path <- M.fromList . flip zip [1 ..] <$> (getOrigin board <&> getPath board)
    return $ length $ concatMap (filter (>=100) . cuts board path) (M.keys path)

cuts :: Board -> Path -> Pos -> [Int]
cuts board path p =  filter (> 0) . fmap (subtract 2 . subtract (path M.! p) . (path M.!)) . filter (`M.member` path) $ jump p

uniq :: [Pos] -> [Pos]
uniq = S.toList . S.fromList

jump = uniq . moves >=> moves

main = getContents >>= print . part1 . parse

module Main where

import Control.Arrow
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Array.Unboxed
import Data.List
import Data.Set qualified as S

type Pos = (Int, Int)
data Dir = N | S | W | E deriving (Show, Eq, Ord)
type Maze = UArray Pos Char
type Problem = RWS Maze () (S.Set (Pos, Dir), S.Set (Int, (Pos, Dir)))

parse = toMaze . lines

toMaze :: [String] -> Maze
toMaze b = listArray ((0, 0), (n - 1, m - 1)) $ concat b
  where
    n = length b
    m = length $ head b

next :: Int -> (Pos, Dir) -> Problem [(Int, (Pos, Dir))]
next c (p, d) = do
    m <- ask

    let straigth = fmap ((1,) . (,d)) . filter ((/= '#') . (m !)) . return $ move d p
        turn = (1000,) . (p,) <$> rot d

    return $ first (+ c) <$> straigth ++ turn

move N = first (subtract 1)
move S = first (+ 1)
move W = second (subtract 1)
move E = second (+ 1)

rot d
    | d `elem` [N, S] = [E, W]
    | otherwise = [N, S]

dijkstra :: MaybeT Problem (Int, (Pos, Dir))
dijkstra = do
    m <- ask
    visited <- gets fst
    Just ((cost, vertex@(p, _)), queue) <- gets (S.minView . snd)

    if m ! p == 'E'
        then return (cost, vertex)
        else
            ( if vertex `S.member` visited
                then modify (second $ const queue)
                else do
                    queue' <- lift $ foldr S.insert queue <$> next cost vertex
                    put (S.insert vertex visited, queue')
            )
                *> dijkstra

part1 b = do
    start <- getStart b
    fst <$> fst (evalRWS (runMaybeT dijkstra) b (S.empty, S.singleton start))

getStart :: Maze -> Maybe (Int, (Pos, Dir))
getStart = fmap ((0,) . (,E) . fst) . find ((== 'S') . snd) . assocs

main = getContents >>= print . part1 . parse

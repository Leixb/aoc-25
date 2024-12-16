module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Array.Unboxed
import Data.List
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S

data Dir = N | S | W | E deriving (Show, Eq, Ord)
type Maze = UArray Pos Char
type Pos = (Int, Int)
type Node = (Pos, Dir)
type CostNode = (Int, Node)
type Problem = RWS Maze [(Node, [Node])] (M.Map Node Int, S.Set (CostNode, Maybe Node))

parse = toMaze . lines

toMaze :: [String] -> Maze
toMaze b = listArray ((0, 0), (n - 1, m - 1)) $ concat b
  where
    n = length b
    m = length $ head b

next :: Int -> (Pos, Dir) -> Problem [CostNode]
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

dijkstra :: MaybeT Problem ()
dijkstra = do
    m <- ask
    visited <- gets fst
    Just (((cost, vertex@(p, _)), father), queue) <- gets (S.minView . snd)

    let (prevCost, visited') = M.insertLookupWithKey (\_ a _ -> a) vertex cost visited

    case prevCost of
        Nothing -> do
            queue' <- lift $ foldr S.insert queue <$> (fmap (,Just vertex) <$> next cost vertex)
            put (visited', queue')
            tell [(vertex, maybeToList father)]
        Just c -> do
            if c == cost
                then tell [(vertex, maybeToList father)]
                else guard $ m ! p /= 'E'
            put (visited, queue)
    dijkstra

solve b = do
    start <- getStart b
    end <- getEnd b
    let ((m, _), w) = execRWS (runMaybeT dijkstra) b (M.empty, S.singleton (start, Nothing))
        parents = M.fromListWith (++) w
        endDirs = (end,) <$> [N, S, E, W]
        min = minimum $ mapMaybe (`M.lookup` m) endDirs
        ends = filter ((== Just min) . (`M.lookup` m)) endDirs
        part2 =
            S.size . S.fromList . fmap fst . concat . takeWhile (not . null) $
                iterate (>>= flip (M.findWithDefault []) parents) ends
    return (min, part2)

getStart :: Maze -> Maybe CostNode
getStart = fmap ((0,) . (,E) . fst) . find ((== 'S') . snd) . assocs

getEnd :: Maze -> Maybe Pos
getEnd = fmap fst . find ((== 'E') . snd) . assocs

main = getContents >>= print . solve . parse

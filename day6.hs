module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.RWS
import Data.Array.Unboxed
import Data.List
import Data.Maybe
import Data.Set qualified as S

type Pos = (Int, Int)
type Grid = UArray Pos Char
data Dir = N | E | S | W deriving (Show)

type Problem = RWS Grid (S.Set Pos) (Pos, Dir)

parse :: String -> Grid
parse s = listArray ((1, 1), (length l, length $ head l)) $ concat l
  where
    l = lines s

getStart :: Grid -> Maybe (Pos, Dir)
getStart = fmap (second getDir) . find ((`elem` "^>v<") . snd) . assocs

getDir '^' = N
getDir 'v' = S
getDir '>' = E
getDir '<' = W

move N = first pred
move S = first succ
move E = second succ
move W = second pred

rot N = E
rot E = S
rot S = W
rot W = N

step :: Problem ()
step = do
    (p, d) <- get
    g <- ask
    let next = move d p
    tell $ S.singleton p
    when (inRange (bounds g) next) $ put ( if g ! next == '#' then (p, rot d) else (next, d)) *> step

part1 = S.size . snd . (evalRWS step <*> (fromJust . getStart))

main = getContents >>= print . part1 . parse

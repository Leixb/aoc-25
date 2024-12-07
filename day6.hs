module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.RWS
import Data.Array.Unboxed
import Data.List
import Data.Maybe
import Data.Set qualified as S
import Debug.Trace

type Pos = (Int, Int)
type Grid = UArray Pos Char
data Dir = N | E | S | W deriving (Show, Ord, Eq)

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
    when (inRange (bounds g) next) $
        modify
            ( if g ! next == '#'
                then second rot
                else first $ const next
            )
            *> step

step' :: (Pos, Dir) -> Problem' ()
step' start = do
    (p, d) <- gets fst
    seen <- gets snd
    g <- ask
    let next = move d p

    when (inRange (bounds g) next) $ do
        if g ! next == '#'
            then modify . first . second $ rot
            else do
                prev <- get

                put (start, S.empty)
                ok <- local (//[(next, '#')]) checkLoop
                when ok $ tell $ S.singleton next

                put prev

                modify . first . first $ const next
        modify . second $ S.insert (p, d)
        step' start

checkLoop :: Problem' Bool
checkLoop = do
    (p, d) <- gets fst
    seen <- gets snd
    g <- ask
    let next = move d p

    if inRange (bounds g) next
        then
            if S.member (p, d) seen
                then return True
                else
                    modify
                        ( ( if g ! next == '#'
                                then second rot
                                else first $ const next
                          )
                            *** S.insert (p, d)
                        )
                        *> checkLoop
        else return False

type Problem' = RWS Grid (S.Set Pos) ((Pos, Dir), S.Set (Pos, Dir))

part1 = S.size . snd . (evalRWS step <*> fromJust . getStart)
part2 g = S.size . snd $ evalRWS (step' start) g (start,S.empty)
    where
        start = fromJust $ getStart g

main = getContents >>= print . (part1 &&& part2) . parse

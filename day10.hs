module Main where

import Control.Arrow
import Control.Monad.Reader
import Data.Array.Unboxed
import Data.List
import qualified Data.Set as S

type Pos = (Int, Int)
type Board = UArray Pos Char
type Prob = Reader Board

parse :: String -> Board
parse s = listArray ((1, 1), (n, m)) $ concat l
  where
    l = lines s
    n = length l
    m = length $ head l

origins :: Prob [Pos]
origins =
    ask >>= \board ->
        return $ fmap fst . filter ((== '0') . snd) $ assocs board

moves :: Pos -> Prob [Pos]
moves pos =
    ask >>= \board ->
        let curr = board ! pos
         in return . filter ((== succ curr) . (board !)) . filter (inRange (bounds board)) $ (.+. pos) <$> deltas
  where
    deltas = [(1, 0), (0, 1), (-1, 0), (0, -1)]
    (ax, ay) .+. (bx, by) = (ax + bx, ay + by)

followTrail :: [Pos] -> Prob [Pos]
followTrail p = do
    board <- ask
    nxt <- concat <$> mapM moves p

    let (nines, rest) = partition ((== '9') . (board !)) nxt

    fmap (++ nines) $ if null rest then return [] else followTrail rest

solve fn = sum . fmap fn . runReader (origins >>= mapM (followTrail . pure))

part1 = solve $ S.size . S.fromList
part2 = solve length

main = getContents >>= print . (part1 &&& part2) . parse

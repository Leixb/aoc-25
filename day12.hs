module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.RWS
import Data.Array.Unboxed
import Data.Monoid
import Data.List
import Data.Ord
import Data.Functor
import Data.Set qualified as S

type Pos = (Int, Int)
type Board = UArray Pos Char
type Prob = RWS Board () (S.Set Pos)

parse :: String -> Board
parse s = listArray ((1, 1), (n, m)) $ concat l
  where
    l = lines s
    n = length l
    m = length $ head l

moves :: Pos -> [Pos]
moves pos = (.+. pos) <$> deltas
  where
    deltas = [(1, 0), (0, 1), (-1, 0), (0, -1)]
    (ax, ay) .+. (bx, by) = (ax + bx, ay + by)

explore :: Pos -> Prob (S.Set Pos, Sum Int)
explore p = do
    board <- ask
    seen <- get

    if S.member p seen
        then return mempty
        else do
            modify $ S.insert p
            let allMoves = S.fromList $ moves p
                (valid, outside) = S.partition (inRange (bounds board)) allMoves
                (equals, diff) = S.partition ((== (board ! p)) . (board !)) valid
                walls = 4 - S.size equals

            neighbors <- mconcat <$> mapM explore (S.toList equals)

            return $ neighbors <> (outside <> diff, Sum 1)

-- exploreAll :: Prob Int
-- exploreAll = sum . fmap (uncurry (*) . (S.size *** getSum)) <$> (ask >>= mapM explore . indices)
exploreAll = filter ((/=0) . snd) . fmap (countSides *** getSum) <$> (ask >>= mapM explore . indices)

-- countSides :: S.Set Pos -> [Pos]
countSides s = y
    where
        l = S.toList s
        -- x = sum . fmap (succ . length . filter (/=(-1)) . diffs . fmap snd) $ groupBy (\a b -> fst a == fst b) $ sort l
        -- y = sum . fmap (succ . length . filter (/=(-1)) . diffs . fmap fst) $ groupBy (\a b -> snd a == snd b) $ sortOn snd l
        -- x = fmap (diffs . fmap snd) $ groupBy (\a b -> fst a == fst b) $ sort l
        -- x = fmap (fmap snd) $ groupBy (\a b -> fst a == fst b) $ sort l
        x = groupBy (\a b -> fst a == fst b) $ sort l
        y = groupBy (\a b -> snd a == snd b) $ sortOn snd l
        -- y = sum . fmap (succ . length . filter (/=(-1)) . diffs . fmap fst) $ groupBy (\a b -> snd a == snd b) $ sortOn snd l

        diffs l = zipWith (-) l (tail l)

part1 = fst . flip (evalRWS exploreAll) S.empty

main = getContents >>= print . part1 . parse

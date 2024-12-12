module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.RWS
import Data.Array.Unboxed
import Data.Monoid
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

moves :: Pos -> Prob [Pos]
moves pos = ask >>= \board -> return . filter (inRange (bounds board)) $ (.+. pos) <$> deltas
  where
    deltas = [(1, 0), (0, 1), (-1, 0), (0, -1)]
    (ax, ay) .+. (bx, by) = (ax + bx, ay + by)

explore :: Pos -> Prob (Sum Int, Sum Int)
explore p = do
    board <- ask
    seen <- get

    if S.member p seen
        then return mempty
        else do
            modify $ S.insert p
            valid <- moves p
            let equals = filter ((== (board ! p)) . (board !)) valid
                walls = 4 - length equals

            neighbors <- mconcat <$> mapM explore equals

            return $ neighbors <> (Sum walls, Sum 1)

exploreAll :: Prob Int
exploreAll = sum . fmap (uncurry (*) . (getSum *** getSum)) <$> (ask >>= mapM explore . indices)

part1 = fst . flip (evalRWS exploreAll) S.empty

main = getContents >>= print . part1 . parse

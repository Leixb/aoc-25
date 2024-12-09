{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Array.ST
import Data.Array.Unboxed
import Data.Char
import Data.List
import Data.Maybe

parse = zip ids . fmap digitToInt . takeWhile (/= '\n')

ids = intersperse Nothing $ Just <$> [0 ..]

expand = foldMap (uncurry $ flip replicate)

process l = runSTArray $ do
    arr <- newListArray (1, length l) l
    getBounds arr >>= uncurry (go arr)
  where
    go arr iL iR = do
        (iL', iR') <- advance arr (iL, iR)
        if iL' < iR'
            then swap arr iL' iR' *> go arr iL' iR'
            else return arr

swap arr i j = do
    a <- readArray arr i
    readArray arr j >>= writeArray arr i
    writeArray arr j a

advance arr (h, t) = (,) <$> advanceHead arr h <*> advanceTail arr t
  where
    advanceHead arr i =
        readArray arr i >>= \case
            Nothing -> return i
            _ -> advanceHead arr (succ i)

    advanceTail arr i =
        readArray arr i >>= \case
            Nothing -> advanceTail arr (pred i)
            _ -> return i

checksum = sum . zipWith (*) [0 ..]

process2 l = runSTArray $ do
    let idxs = scanl' (+) 1 $ snd <$> l
        iR = last idxs
    arr <- newArray (1, iR) Nothing
    forM_ (zip idxs l) $ \(i, v) -> writeArray arr i (Just v)

    runMaybeT $ go arr iR

    return arr
  where
    go :: MArr s -> Int -> MaybeT (ST s) ()
    go arr iR = do
        (i, sz) <- findVal arr iR
        (findGap arr sz 1 >>= move arr i) <|> return ()
        go arr $ pred i

type MArr s = STArray s Int (Maybe (Maybe Int, Int))

findGap :: MArr s -> Int -> Int -> MaybeT (ST s) Int
findGap arr n i = do
    mx <- lift $ snd <$> getBounds arr
    guard $ i <= mx
    ( do
            Just (Nothing, v) <- lift (readArray arr i)
            guard $ v >= n
            hoistMaybe $ Just i
        )
        <|> findGap arr n (succ i)

findVal :: MArr s -> Int -> MaybeT (ST s) (Int, Int)
findVal arr i = do
    guard $ i >= 1
    lift (readArray arr i) >>= \case
        Just (Just _, sz) -> hoistMaybe $ Just (i, sz)
        _ -> findVal arr $ pred i

move arr iVal iGap = do
    guard $ iGap < iVal

    Just (Nothing, gap) <- lift $ readArray arr iGap
    v@(Just (Just _, sz)) <- lift $ readArray arr iVal
    lift . writeArray arr iVal $ Just (Nothing, sz)
    lift $ writeArray arr iGap v

    when (gap > sz) . lift . writeArray arr (iGap + sz) $ Just (Nothing, gap - sz)

part1 = checksum . catMaybes . elems . process . expand
part2 = checksum . fmap (fromMaybe 0) . expand . catMaybes . elems . process2

main = getContents >>= print . (part1 &&& part2) . parse

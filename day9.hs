{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe

parse = zip ids . fmap digitToInt . takeWhile (/= '\n')

ids = intersperse Nothing $ Just <$> [0 ..]

expand = foldMap (uncurry $ flip replicate)

process l = runSTArray $ do
    arr <- newListArray (1, length l) l
    (idxL, idxR) <- getBounds arr
    go arr idxL idxR
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

checkSum = sum . zipWith (*) [0..] . catMaybes . elems

main = getContents >>= print . checkSum . process . expand . parse

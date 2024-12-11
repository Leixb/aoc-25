module Main where

import Control.Arrow
import Data.Monoid

data Tree a = Tree (Tree a) a (Tree a)

-- https://stackoverflow.com/questions/3208258
memo1 f = index nats
  where
    nats = go 0 1
    go i s = Tree (go (i + s) s') (f i) (go (i + s') s')
      where
        s' = 2 * s
    index (Tree l v r) i
        | i < 0 = f i
        | i == 0 = v
        | otherwise = case (i - 1) `divMod` 2 of
            (i', 0) -> index l i'
            (i', 1) -> index r i'

memo2 f = memo1 (memo1 . f)

blink = memo2 blink'
  where
    blink' c n
        | c == 0 = 1
        | n == 0 = blink c' 1
        | even digits = blink c' l <> blink c' r
        | otherwise = blink c' $ n * 2024
      where
        digits = succ . floor . logBase 10 . fromIntegral $ n
        (l, r) = n `divMod` (10 ^ (digits `div` 2))
        c' = pred c

doBlinks n = getSum . mconcat . fmap (blink n)
part1 = doBlinks 25
part2 = doBlinks 75

main = getContents >>= print . (part1 &&& part2) . fmap read . words

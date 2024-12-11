module Main where

blink :: Int -> [Int]
blink n
    | n == 0 = [1]
    | even digits = [ l, r ]
    | otherwise = [n * 2024]
  where
    digits = succ . floor . logBase 10 . fromIntegral $ n
    (l, r) = n `divMod` (10 ^ (digits `div` 2))


main = getContents >>= print . length . (!! 25) . iterate (concatMap blink)  . fmap read . words

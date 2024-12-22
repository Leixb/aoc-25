module Main where

import Control.Arrow
import Data.Bits

toSecret :: Int -> Int
toSecret = step1 >>> step2 >>> step3
  where
    step1 n = ((n `shiftL` 06) `xor` n) .&. 0xFFFFFF
    step2 n = ((n `shiftR` 05) `xor` n) .&. 0xFFFFFF
    step3 n = ((n `shiftL` 11) `xor` n) .&. 0xFFFFFF

secretNums = iterate toSecret

main = getContents >>= print . sum . fmap ((!! 2000) . secretNums . read) . lines

module Main where

import Control.Arrow
import Data.Char
import Text.ParserCombinators.ReadP

numP = read <$> munch1 isDigit
parse = endBy ((,) <$> (numP <* string ": ") <*> sepBy numP (char ' ')) (char '\n')

valid n [m] = m == n
valid n (x : xs) = n > 0 && valid (n - x) xs || (n `mod` x) == 0 && valid (n `div` x) xs

part1 = sum . fmap fst . filter (uncurry valid . second reverse)

concatNum r = (+r) . (* 10 ^ digits r)
    where
        digits = succ . floor . logBase 10 . fromIntegral

allPossible [n] = [n]
allPossible (x:xs) = ((x+) <$> rest) ++ ((x*) <$> rest) ++ (concatNum x <$> rest)
    where
        rest = allPossible xs

part2 = sum . fmap fst . filter (uncurry elem . second (allPossible . reverse))

main = getContents >>= print . (part1 &&& part2) . fst . last . readP_to_S parse

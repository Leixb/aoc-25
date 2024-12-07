module Main where

import Text.ParserCombinators.ReadP
import Data.Char
import Control.Arrow

num :: ReadP Integer
num = read <$> munch1 isDigit
parse = endBy  ((,) <$> (num  <* string ": ") <*> sepBy num (char ' ')) (char '\n')

valid :: Integer -> [Integer] -> Bool
valid n [] = error "invalid"
valid n [m] = m == n
valid n (x:xs)
    | n <= 0 = False
    | otherwise = valid (n-x) xs || ((n `mod` x) == 0 && valid (n `div` x) xs)

part1 = uncurry valid . second reverse

main = getContents >>= print . sum . fmap fst . filter part1 . fst . last . readP_to_S parse

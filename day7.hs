module Main where

import Control.Arrow
import Data.Char
import Text.ParserCombinators.ReadP

numP = read <$> munch1 isDigit
parse = fst . last . readP_to_S (endBy ((,) <$> (numP <* string ": ") <*> sepBy numP (char ' ')) (char '\n'))

valid n [m] = m == n
valid n (x : xs) = n > 0 && valid (n - x) xs || ((n `mod` x) == 0 && valid (n `div` x) xs)

part1 = sum . fmap fst . filter (uncurry valid . second reverse)

main = getContents >>= print . part1 . parse

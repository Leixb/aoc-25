module Main where

import Data.List

import Text.ParserCombinators.ReadP
import Data.Char

parse = fst . last . readP_to_S ((,) <$> (patterns <* eol <* eol) <*> designs)
  where
    eol = char '\n'
    patterns = sepBy word (string ", ")
    designs = endBy word eol
    word = munch1 isLetter

part1 patterns = filter (valid patterns)

dropPrefix = drop . length

valid patterns "" = True
valid patterns design = case filter (`isPrefixOf` design) patterns of
    [] -> False
    l -> any (valid patterns . (`dropPrefix` design)) l

main = getContents >>= print . length . uncurry part1 . parse

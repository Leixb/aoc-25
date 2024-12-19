{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List

import Control.Arrow
import Control.Monad.State
import Data.Char
import Data.Map qualified as M
import Data.Monoid
import Text.ParserCombinators.ReadP

parse = fst . last . readP_to_S ((,) <$> (patterns <* eol <* eol) <*> designs)
  where
    eol = char '\n'
    patterns = sepBy word (string ", ")
    designs = endBy word eol
    word = munch1 isLetter

part1 patterns = length . filter (valid patterns)

part2 patterns = getSum . combinations patterns

dropPrefix = drop . length

valid :: [String] -> String -> Bool
valid patterns design = go design
  where
    go "" = True
    go design = case filter (`isPrefixOf` design) patterns of
        [] -> False
        l -> any (go . (`dropPrefix` design)) l

combinations :: [String] -> [String] -> Sum Int
combinations patterns designs = evalState (fmap mconcat . mapM go $ designs) mempty
  where
    go "" = return $ Sum 1
    go design =
        gets (M.lookup design) >>= \case
            Just c -> return c
            Nothing -> case filter (`isPrefixOf` design) patterns of
                [] -> return $ Sum 0
                l -> do
                    res <- mconcat <$> mapM (go . (`dropPrefix` design)) l
                    modify (M.insert design res)
                    return res

main = getContents >>= print . (uncurry part1 &&& uncurry part2) . parse

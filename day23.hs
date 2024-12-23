module Main where

import Control.Arrow
import Data.Map qualified as M
import Data.Set qualified as S
import Text.ParserCombinators.ReadP
import Data.Tuple

parse = toAdjMap . fst . last . readP_to_S (endBy ((,) <$> (count 2 get <* char '-') <*> count 2 get) (char '\n') <* eof)
  where
    toAdjMap = M.fromListWith S.union . (second S.singleton <$>) . ((++) <*> fmap swap)

k3 g n = S.unions $ uncurry buildSets . (id &&& S.intersection neighbours . (g M.!)) <$> S.toList neighbours
  where
    neighbours = g M.! n
    buildSets b = S.map (S.insert n . S.insert b . S.singleton)

part1 g = S.size . S.unions $ k3 g <$> filter ((== 't') . head) (M.keys g)

main = getContents >>= print . part1 . parse

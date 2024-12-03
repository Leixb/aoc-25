module Main where

import Control.Arrow hiding ((+++))
import Data.Char
import Data.Functor
import Data.Maybe
import Text.ParserCombinators.ReadP hiding (get)
import Text.ParserCombinators.ReadP qualified as P

data Op = Mul Int Int | Do | Dont deriving (Show)

parser1 :: ReadP [(Int, Int)]
parser1 = catMaybes <$> many ((Just <$> mul) <++ (P.get $> Nothing))

parser2 :: ReadP [Op]
parser2 = catMaybes <$> many ((Just <$> operation) <++ (P.get $> Nothing))

mul :: ReadP (Int, Int)
mul =
    (,) <$> (string "mul(" *> (read <$> munch1 isDigit <* char ',')) <*> (read <$> munch1 isDigit <* char ')')

operation :: ReadP Op
operation = (string "do()" $> Do) +++ (string "don't()" $> Dont) +++ (uncurry Mul <$> mul)

foldOp :: (Bool, Int) -> Op -> (Bool, Int)
foldOp (_, n) Do = (True, n)
foldOp (_, n) Dont = (False, n)
foldOp (True, n) (Mul a b) = (True, n + a * b)
foldOp (False, n) _ = (False, n)

part1 = sum . fmap (uncurry (*)) . fst . last . readP_to_S parser1
part2 = snd . foldl foldOp (True, 0) . fst . last . readP_to_S parser2

main = getContents >>= print . (part1 &&& part2)

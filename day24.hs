module Main where

import Control.Monad.RWS
import Data.Char (digitToInt)
import Data.Functor
import Data.Map qualified as M
import Data.Maybe
import Text.ParserCombinators.ReadP hiding (get)
import Text.ParserCombinators.ReadP qualified as ReadP
import Data.List
import Data.Bits (shiftL)

type Cable = String

data Connection = And Cable Cable | Or Cable Cable | Xor Cable Cable deriving (Show)

cable = count 3 ReadP.get
eol = char '\n'
initial :: ReadP (M.Map String Bool)
initial = M.fromList <$> endBy ((,) <$> cable <*> (string ": " *> (toEnum . digitToInt <$> ReadP.get))) eol
wires = M.fromList <$> endBy wire eol

wire = do
    a <- cable <* char ' '
    op <- choice [string "AND" $> And, string "OR" $> Or, string "XOR" $> Xor]
    b <- char ' ' *> cable
    c <- string " -> " *> cable
    return (c, op a b)

parse = fst . last . readP_to_S ((,) <$> initial <*> (eol *> wires <* eof))

type Problem = RWS (M.Map Cable Connection) () (M.Map Cable Bool)

getConnection :: Connection -> Problem Bool
getConnection (And a b) = (&&) <$> getWire a <*> getWire b
getConnection (Or a b) = (||) <$> getWire a <*> getWire b
getConnection (Xor a b) = xor <$> getWire a <*> getWire b

xor True False = True
xor False True = True
xor _ _ = False


getWire :: Cable -> Problem Bool
getWire cable = do
    let computed = do
            a <- asks (M.! cable) >>= getConnection
            modify (M.insert cable a)
            return a
    gets (M.!? cable) >>= maybe computed return

part1 :: M.Map Cable Bool -> M.Map Cable Connection -> Int
part1 initial wiring = sum . fmap fst . filter snd . zip (iterate (`shiftL` 1) 1) $ fst $ evalRWS (mapM getWire zs) wiring initial
  where
    zs = filter ((=='z') . head) $ sort $ M.keys wiring

main = getContents >>= print . uncurry part1 . parse

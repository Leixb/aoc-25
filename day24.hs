module Main where

import Control.Arrow
import Control.Monad.RWS
import Data.Bits (shiftL)
import Data.Char (digitToInt)
import Data.Functor
import Data.List
import Data.Map qualified as M
import Data.Tuple
import Text.ParserCombinators.ReadP hiding (get)
import Text.ParserCombinators.ReadP qualified as ReadP

type Cable = String
data Connection = And Cable Cable | Or Cable Cable | Xor Cable Cable deriving (Show)

parse = fst . last . readP_to_S ((,) <$> initial <*> (eol *> wires <* eof))
  where
    cable = count 3 ReadP.get
    eol = char '\n'
    initial :: ReadP (M.Map Cable Bool)
    initial = M.fromList <$> endBy ((,) <$> cable <*> (string ": " *> (toEnum . digitToInt <$> ReadP.get))) eol
    wires =
        M.fromList
            <$> (`endBy` eol)
                ( do
                    a <- cable <* char ' '
                    op <- choice [string "AND" $> And, string "OR" $> Or, string "XOR" $> Xor]
                    b <- char ' ' *> cable
                    c <- string " -> " *> cable
                    return (c, op a b)
                )

type Problem = RWS (M.Map Cable Connection) () (M.Map Cable Bool)

getConnection :: Connection -> Problem Bool
getConnection (And a b) = (&&) <$> getWire a <*> getWire b
getConnection (Or a b) = (||) <$> getWire a <*> getWire b
getConnection (Xor a b) = xor <$> getWire a <*> getWire b
  where
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

fromBin :: [Bool] -> Int
fromBin = sum . fmap fst . filter snd . zip (iterate (`shiftL` 1) 1)

toBin :: Int -> [Bool]
toBin = unfoldr (\v -> if v == 0 then Nothing else Just (first (== 1) (swap (divMod v 2))))

part1 initial wiring = fst $ evalRWS (mapM getWire zs) wiring initial
  where
    zs = filter ((== 'z') . head) . sort $ M.keys wiring

-- Check differences in bits. Use this to manually inspect the wire graph and
-- fix it.
part2 initial wiring = fmap fst . filter snd $ zip [0 ..] (zipWith (/=) got expect)
  where
    xs = fromBin . fmap (initial M.!) . filter ((== 'x') . head) . sort $ M.keys initial
    ys = fromBin . fmap (initial M.!) . filter ((== 'y') . head) . sort $ M.keys initial
    zs = filter ((== 'z') . head) . sort $ M.keys wiring

    got = part1 initial wiring
    expect = toBin $ xs + ys

main = getContents >>= print . (fromBin . uncurry part1 &&& uncurry part2) . parse

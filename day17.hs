{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.Char
import Data.Text.Read
import Text.ParserCombinators.ReadP hiding (get)
import Debug.Trace

data Register = A | B | C
data Registers = Registers {a :: Int, b :: Int, c :: Int} deriving (Show)
data Op = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV deriving (Show, Enum)
data Instr = Instr Op Int deriving (Show)

type Problem = RWS [Instr] [Int] (Registers, Int)

parse = fst . last . readP_to_S ((,) <$> registers <*> (string "\nProgram: " *> sepBy instr (char ',') <* char '\n'))
  where
    num = read <$> munch1 isDigit
    register c = string "Register " *> char c *> string ": " *> num <* char '\n'
    instr = Instr <$> (toEnum <$> num) <*> (char ',' *> num)
    registers = Registers <$> register 'A' <*> register 'B' <*> register 'C'

store :: Register -> Int -> Problem ()
store A n = modify . first $ \r -> r{a = n}
store B n = modify . first $ \r -> r{b = n}
store C n = modify . first $ \r -> r{c = n}

combo :: Int -> Problem Int
combo 4 = gets (a . fst)
combo 5 = gets (b . fst)
combo 6 = gets (c . fst)
combo n = return n

exec :: MaybeT Problem ()
exec = do
    (Registers{a, b, c}, ip) <- fmap traceShowId $ get
    program <- ask
    guard $ ip < length program

    lift $ case (traceShowId $ program !! ip) of
        Instr ADV n -> combo n >>= store A . (a `shiftR`)
        Instr BXL n -> store B (b `xor` n)
        Instr BST n -> combo n >>= store B . (.&. 0b111)
        Instr JNZ n -> when (a /= 0) $ modify . second . const . pred $ n `div` 2
        Instr BXC _ -> store B $ b `xor` c
        Instr OUT n -> combo n >>= tell . pure . (.&. 0b111)
        Instr BDV n -> combo n >>= store B . (a `shiftR`)
        Instr CDV n -> combo n >>= store C . (a `shiftR`)

    modify (second succ)
    exec

part1 registers program = execRWS (runMaybeT exec) program (registers, 0)

-- main = getContents >>= print . parse
main = getContents >>= print . uncurry part1 . parse

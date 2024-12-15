{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Array.Unboxed
import Data.List
import Data.Maybe

type Pos = (Int, Int)
type Board = UArray Pos Char

data Dir = U | D | L | R deriving (Show)
type Problem = State (Pos, Board)

toDir '^' = U
toDir 'v' = D
toDir '<' = L
toDir '>' = R

move U = first (subtract 1)
move D = first (+ 1)
move L = second (subtract 1)
move R = second (+ 1)

toBoard :: [String] -> Board
toBoard b = listArray ((0, 0), (n - 1, m - 1)) $ concat b
  where
    n = length b
    m = length $ head b

findOrigin :: Board -> Maybe Pos
findOrigin = fmap fst . find ((== '@') . snd) . assocs

findHole :: Pos -> Dir -> Problem (Maybe Pos)
findHole p d = do
    b <- gets snd
    case b ! p of
        '#' -> return Nothing
        '.' -> return $ Just p
        'O' -> findHole (move d p) d

doMove :: Dir -> Problem ()
doMove d = do
    (origin, board) <- get
    let next = move d origin
    findHole next d >>= \case
        Nothing -> return ()
        Just hole -> modify $ const next *** (// [(next, board ! hole), (hole, board ! next)])

part1 b dirs = execState (mapM_ doMove dirs) (origin, b // [(origin, '.')])
  where
    origin = fromJust $ findOrigin b

calcGPS = sum . fmap (uncurry (+) . first (* 100) . fst) . filter ((== 'O') . snd) . assocs

parse = (toBoard *** fmap toDir . concat . tail) . break (== "") . lines

main = getContents >>= print . calcGPS . snd . uncurry part1 . parse

display :: Board -> IO ()
display b = do
    let ((y0, x0), (yn, xm)) = bounds b
    forM_ [y0 .. yn] $ \y -> do
        forM_ [x0 .. xm] $ \x -> do
            putChar (b ! (y, x))
        putChar '\n'

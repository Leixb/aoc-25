{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either
import Data.Text hiding (all, head, zipWith)
import Data.Text qualified as T
import Data.Text.IO as TIO

type Pins = [Int]

toKeyLock :: [Text] -> Either Pins Pins
toKeyLock v = (if T.head (head v) == '#' then Left else Right) $ fmap (pred . count "#") v

solve keys locks = sum [1 | k <- keys, l <- locks, fit k l]
  where
    fit a b = all (<= 5) $ zipWith (+) a b

main = TIO.getContents >>= print . uncurry solve . partitionEithers . fmap (toKeyLock . transpose . T.lines) . splitOn "\n\n"

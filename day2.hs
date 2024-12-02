import Control.Arrow
import Control.Monad

main = getContents >>= print . (part1 &&& part2) . fmap (fmap read . words) . lines

part1 = length . filter isSafe
part2 = length . filter (any isSafe . removeOne)

isSafe = ap (zipWith (-)) tail >>> (all (between 1 3) &&& all (between (-3) (-1))) >>> uncurry (||)
 where
  between a b = (a <=) &&& (<= b) >>> uncurry (&&)

removeOne [] = []
removeOne (x : xs) = xs : fmap (x :) (removeOne xs)

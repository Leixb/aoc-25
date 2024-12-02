import Control.Arrow

main = getContents >>= print . (part1 &&& part2) . parse

parse = fmap (fmap read . words) . lines

part1 = length . filter id . fmap isSafe
part2 = length . filter id . fmap (any isSafe . removeOne)

isSafe l = all (between 1 3) l' || all (between (-3) (-1)) l'
  where
    l' = zipWith (-) l (tail l)
    between a b n = a <= n && n <= b

removeOne [] = []
removeOne (x:xs) = xs : fmap (x :) (removeOne xs)

main = getContents >>= print . part1 . parse

parse = fmap (fmap read . words) . lines

part1 = length . filter id . fmap checkLine
checkLine l = (pos || neg) && all (between 1 3 . abs) l'
  where
    l' = zipWith (-) l (tail l)
    neg = all (<0) l'
    pos = all (>0) l'
    between a b n = a <= n && n <= b

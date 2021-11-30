msort :: (Ord a) => [a] -> [a]
msort [] = []
msort (a:[]) = [a]
msort (a:b:y)
        | a < b || a == b = merge [a, b] z
        | a > b = merge [b, a] z
        where z = msort y

merge :: (Ord a) => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge (as@(a:_)) (bs@(b:_))
        | a < b || a == b = a:merge (tail as) bs
        | a > b = b:merge (tail bs) as

-- Quicksort picks a pivot, in this case the first element, and puts all
-- elements less than the pivot to its left, and all elements greater than
-- or equal to the pivot to its right. This process is continued until there is
-- one or no elements left
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (a:as) = (qsort lt) ++ (p:(qsort gte))
        where (lt, p, gte) = partition [] a [] as

partition :: (Ord a) => [a] -> a -> [a] -> [a] -> ([a], a, [a])
partition lt p gte (x:xs)
        | x < p = partition (x:lt) p gte xs
        | otherwise = partition lt p (x:gte) xs
partition lt p gte [] = (lt, p, gte)

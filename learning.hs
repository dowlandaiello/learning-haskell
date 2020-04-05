import Data.List

main = do
    -- Basic arithmetic
    putStrLn "Some arithmetic: "

    putStrLn (show (2 + 15))
    putStrLn (show (49 * 100))
    putStrLn (show (1892 - 1472))
    putStrLn (show (5 / 2))

    -- Boolean algebra
    putStrLn "\nSome boolean algebra: "

    putStrLn (show (True && False))
    putStrLn (show (True && True))
    putStrLn (show (False || True))
    putStrLn (show (not False))
    putStrLn (show (not (True && True)))

    -- Testing for equality
    putStrLn "\nTesting for equality: "

    putStrLn (show (5 == 5))
    putStrLn (show (1 == 0))
    putStrLn (show (5 /= 5))
    putStrLn (show (5 /= 4))

    putStrLn ("\nsucc(69): " ++ (show (succ 69)))

    putStrLn ("factorial(4): " ++ (show (factorial 4)))

    let myItems = [1, 2, 3]

    putStrLn ("lastElement of "++show myItems++" is "++(show (lastElement myItems)))

    putStrLn ("secondToLast elemtn of "++show myItems++" is "++(show (secondToLast myItems)))

    putStrLn ("element 2 of "++show myItems++" is "++(show (elementAt myItems 2)))

    putStrLn ("length of "++show myItems++" is "++(show (myLength myItems)))

    putStrLn (show myItems++" reversed is "++(show (reverse myItems)))

    putStrLn (show myItems++" is a palindrome: "++(show (isPalindrome myItems)))
    putStrLn (show [1,2,1]++" is a palindrome: "++(show (isPalindrome [1,2,1])))

    let complex = List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]

    putStrLn ("flattened: "++(show (flatten complex)))

    putStrLn (show [1,1,1,2,2,3,4,5]++" without repeats: "++show (removeDuplicates [1,1,1,2,2,3,4,5]))

    return Nothing

lastElement x = last x

secondToLast x = last (init x)

elementAt x i = x !! i

myLength x = length x

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)


isPalindrome :: [Int] -> Bool
isPalindrome x
    | length x <= 1 = True
    | head x == last x = isPalindrome (init (tail x))
    | otherwise = False

data NestedList a = Elem a | List [NestedList a] | None

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List x) = concat (map (\item -> flatten item) x)

removeDuplicates list
    | length list <= 1 = []
    | otherwise = if head list == head (tail list)
        then removeDuplicates (tail list)
        else concat [[head list], removeDuplicates (tail list)]

pack :: [a] -> NestedList a
pack list
    | length list == 0 = None
    | length list == 1 = Elem (head list)
    | otherwise = []

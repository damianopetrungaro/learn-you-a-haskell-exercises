{-
 -Once you've installed Haskell from http://www.haskell.org/platform/, load the interpreter with the command ghci.
 -
 -You can load (and reload) this file in the interpreter with the command: ":l 2-starting-out.hs"
 -
 -The first function has been completed as an example. All the other functions are undefined.
 -They can be implemented in one line using the material covered in http://learnyouahaskell.com/starting-out
 -
 -All indices are zero based.
 -}

-- Find the penultimate element in list l
penultimate l = last (init l)

-- Find the element at index k in list l
-- For example: "findK 2 [0,0,1,0,0,0]" returns 1
findK :: (Show a) => Int -> [a] -> a
findK k l = l !! k

-- Determine if list l is a palindrome
-- isPalindrome l = l == reverse l
isPalindrome :: (Eq a, Show a) => [a] -> Bool
isPalindrome l
    | length l `elem` [2,3] = head l == last l
    | length l > 1 = if head l == last l then isPalindrome(tail(init l)) else False
    | otherwise = True

{-
 - Duplicate the elements in list xs, for example "duplicate [1,2,3]" would give the list [1,1,2,2,3,3]
 - Hint: The "concat [l]" function flattens a list of lists into a single list.
 - (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
 -
 - For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
 -}
duplicate :: (Show a) => [a] -> [a]
duplicate xs
    | length xs > 1 = headOfList ++ headOfList ++ duplicate(tail xs)
    | length xs == 1 = headOfList ++ headOfList
    where
        headOfList = take 1 xs

{-
 - Imitate the functinality of zip
 - The function "min x y" returns the lower of values x and y
 - For example "ziplike [1,2,3] ['a', 'b', 'c', 'd']" returns [(1,'a'), (2, 'b'), (3, 'c')]
 -}
ziplike :: (Show a, Show b) => [a] -> [b] -> [(a,b)]
ziplike xs ys
   | length xs > 1 && length xs > 1 = [(head xs, head ys)] ++ ziplike (tail xs) (tail ys)
   | otherwise = [(head xs, head ys)]

-- Split a list l at element k into a tuple: The first part up to and including k, the second part after k
-- For example "splitAtIndex 3 [1,1,1,2,2,2]" returns ([1,1,1],[2,2,2])
splitAtIndex :: (Show a) => Int -> [a] -> ([a],[a])
splitAtIndex k l = (take k l, drop k l)

-- Drop the element at index k in list l
-- For example "dropK 3 [0,0,0,1,0,0,0]" returns [0,0,0,0,0,0]
dropK :: (Show a) => Int -> [a] -> [a]
dropK k l = take k l ++ drop (k + 1) l

-- Extract elements between ith and kth element in list l. Including i, but not k
-- For example, "slice 3 6 [0,0,0,1,2,3,0,0,0]" returns [1,2,3]
slice :: (Show a) => Int -> Int -> [a] -> [a]
slice i k l = let dropped = drop i l in take (k - i) dropped

-- Insert element x in list l at index k
-- For example, "insertElem 2 5 [0,0,0,0,0,0]" returns [0,0,0,0,0,2,0]
insertElem :: (Show a) =>  a -> Int -> [a] -> [a]
insertElem x k l = (take k l) ++ [x] ++ (drop k l)

-- Rotate list l n places left.
-- For example, "rotate 2 [1,2,3,4,5]" gives [3,4,5,1,2]
rotate :: (Show a) => Int -> [a] -> [a]
rotate n l = drop n l ++ take n l


main = do
    print $ findK 2 [0,1,2,3,4,5]
    print $ findK 2 [0,1,2,3,4,5]
    print $ isPalindrome [0,1,0,1]
    print $ isPalindrome [1,1,1,1]
    print $ isPalindrome "CavavaC"
    print $ isPalindrome "A"
    print $ isPalindrome "aa"
    print $ duplicate [1,2,3]
    print $ ziplike [1,2,3] ["a", "b", "c"]
    print $ splitAtIndex 3 [1,1,1,2,2,2]
    print $ dropK 3 [0,0,0,1,0,0,0]
    print $ slice 3 6 [0,0,0,1,2,3,0,0,0]
    print $ insertElem 2 5 [0,0,0,0,0,0]
    print $ rotate 2 [1,2,3,4,5]

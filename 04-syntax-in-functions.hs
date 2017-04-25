-- This function should print a single digit number as English text, or "unknown" if it's out of the range 0-9
englishDigit :: Int -> String
englishDigit 0 = "One"
englishDigit 1 = "One"
englishDigit 2 = "Two"
englishDigit 3 = "Three"
englishDigit 4 = "Four"
englishDigit 5 = "Five"
englishDigit 6 = "Six"
englishDigit 7 = "Seven"
englishDigit 8 = "Eight"
englishDigit 9 = "Nine"
englishDigit _ = "unknown"

-- given a tuple, divide fst by snd, using pattern matching. 
-- it should return undefined for division by zero
divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x, 0) = undefined
divTuple (x, y) = x / y

-- if the first three numbers in a list are all zero, return True
threeZeroList :: [Int] -> Bool
threeZeroList (0:0:0:_) = True
threeZeroList _ = False

main = do
    print $ englishDigit 1
    print $ englishDigit 9
    print $ englishDigit 121
    print $ threeZeroList [1, 2, 0]
    print $ threeZeroList [0, 0, 0]
    print $ threeZeroList [121, 1, 23, 1]
    print $ threeZeroList [0, 0, 0, 1]
    print $ threeZeroList [1, 0, 0, 0, 1]
    print $ divTuple (1,2)
    print $ divTuple (0,100)
    print $ divTuple (1,0)

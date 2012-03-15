-- Thu Mar 15 18:42:49 EDT 2012
-- http://www.haskell.org/haskellwiki/99_questions/21_to_28
-- Solution 21

-- Solution to Problem 21
insertAt :: (Eq a) => a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x ys n = left ++ [x] ++ right
    where (left, right) = splitAt (n-1) ys

-- Solution to Problem 22
range :: Int -> Int -> [Int]
range x y = if x > y
            then []
            else
                x : range (succ x) y

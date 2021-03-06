-- Sat Jan 21 10:36:25 EST 2012
-- Some exercises to practice stuff that I have learnt in the
-- first 6 chapters of LYAH
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10

-- Solution to Problem 1
myLast :: [a] -> a
myLast [] = error "Cant find last element of an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' = head . reverse

-- Solution to Problem 2
myButLast :: [a] -> a
myButLast [] = error "Cant find one but last element of an empty list"
myButLast (x:[]) = error "Cant find one but last element of a singleton list"
myButLast (x:_:[]) = x
myButLast (x:xs) = myButLast xs

myButLast' :: [a] -> a
myButLast' = head . tail . reverse

-- Solution to Problem 3
-- elementAt :: (Num b) => [a] -> b -> a
elementAt :: [a] -> Int -> a
elementAt [] _ = error "List too small"
elementAt (x:_) 1 = x
elementAt (x:xs) pos = elementAt xs (pos -1)

-- Solution to Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Solution to Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]


-- Solution to Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = if xs == myReverse xs 
			then True
			else False

-- Solution to Problem 7
{-data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)
myFlatten (List []) = []-}

-- This problem is not clearly stated
-- Inituitively, you want to solve flattening 
-- any list of arbitrary depth
-- Googling, that's possible by using Data.Tree

data NestedList a = Elem a | List [NestedList a]
 
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- *** I DID NOT READ THE PROBLEM CORRECTLY ***
-- *** I solved converting an ordered list to an ordered set **
-- Not Solution to Problem 8
-- This solution does not retain the order !
addIfNotPresent :: (Ord a) => a -> [a] -> [a]
addIfNotPresent y [] = [y]
addIfNotPresent y xs = if y `elem` xs
		       	then xs
			else xs ++ [y]

compressor :: (Ord a) => [a] -> [a] -> [a]
compressor [] [] = []
compressor [] (c:[]) = [c]
compressor (x:xs) cs = addIfNotPresent x $ compressor xs cs

compress :: (Ord a) => [a] -> [a]
compress xs = compressor xs []

-- Solution to Problem 8
-- attempting to retaining the order intact 

compressor' :: (Ord a) => [a] -> [a] -> [a]
compressor' [] [] = []
compressor' cs [] = cs
compressor' cs (x:xs) = compressor' (addIfNotPresent x cs) xs 

compress' :: (Ord a) => [a] -> [a]
compress' xs = compressor' [] xs

-- Solution to Problem 8

sol8 :: Eq a => [a] -> [a]
sol8 []     = []
sol8 (x:xs) = x : (sol8 $ dropWhile (== x) xs)

-- Alternate Solution to Problem 8

sol8' :: (Eq a) => [a] -> [a]
sol8' (x:ys@(y:_))
	| x == y = sol8' ys
	| otherwise = x : sol8' ys
sol8' ys = ys

-- Solution to Problem 9
{-isSame :: (Eq a) => [a] -> a -> Bool
isSame [] _ = False
isSame xs y = if y == head xs 
		then True
		else False

pack :: (Eq a) => [a] -> [[a]]
pack (x:y:ys)
	| x == y = [x,y] ++ pack ys
	| otherwise = [x] : pack (y:ys)


pack :: (Eq a) => [a] -> [[a]]
pack []  = [[]]
pack (x:[]) = [[x]]
pack (x:y:[])
	| x == y = [[x,y]]
	| otherwise = [[x],[y]]
pack (x:xs@(y:ys))
	| x == y =  [[x,y]] ++ pack ys
	| otherwise [[x]] ++ pack xs-}

-- Solution 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = 	if x `elem` (head (pack xs))
		then (x:(head (pack xs))):(tail (pack xs))
		else [x]:(pack xs)
-- Solution 10
encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode [x] = [(1,x)]
encode xs = map f (pack xs) where
		f ys = (length ys, head ys)


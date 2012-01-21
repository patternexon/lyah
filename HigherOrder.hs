-- Chapter 6
-- Higher Order Functions

multThree :: (Num a) => a -> (a -> (a -> a))
multThree x y z = x * y * z

-- Num because a has to be a number to be compared with 100
-- Ord because we are using 'compare' to compare 100 and x
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100 

dividebyTen :: (Floating a) => a -> a
dividebyTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

{-flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
	where g x y = f y x
-}

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
	| p x = x : filter' p xs
	| otherwise = filter' p xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
	let smallerSorted = quicksort (filter (<= x) xs)
	    biggerSorted = quicksort (filter (> x) xs)
	in  smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
	where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x 
	| even x = x : chain (x `div` 2)
	| odd x = x : chain (3*x + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
	where isLong xs =  length xs > 15

-- This was the first time I thought through 
-- while building the function
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) 
	| not (p x) = takeWhile' p []
	| otherwise = x : takeWhile' p xs  	

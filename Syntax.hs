-- Chapter 4
-- Syntax in Functions

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you are out of luck!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "More than 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors x y = (fst x + fst y, snd x + snd y) 

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "This list has one element which is " ++ show x
tell (x:y:[]) = "This list has 2 element which are " ++ show x ++ " and " ++ show y
tell (x:y:xs) = "This list is long"

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Interesting things begin now

{-bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
	| bmi <= 18.5 = "You are underweight"
	| bmi <= 25.0 = "You are apparently normal"
	| bmi <= 30.0 = "You are overweight" 
	| otherwise = "Fat as hell!"
-}

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' w h
	| w/h^2 <= 18.5 = "You are underweight"
	| w/h^2 <= 25.0 = "You are apparently normal"
	| w/h^2 <= 30.0 = "You are overweight" 
	| otherwise = "Fat as hell!"

-- Why cant we overload "bmiTell bmi" and "bmiTell w h" as we can in C like languages?
-- Because of haskell's laziness that lets one create a partial function -
-- "bmiTell w" can also be a definition of the function "bmiTell" 
-- Since the compiler cannot distinguish between these 2 definitions this kind of overloading
-- is not possible. 

max' :: (Ord a) => a -> a -> a
max' a b 
	| a > b = a
	| otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
	| a == b = EQ
	| a > b = GT
	| a < b = LT

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell w h
	| bmi <= skinny = "You are underweight"
	| bmi <= normal = "You are apparently normal"
	| bmi <= fat = "You are overweight" 
	| otherwise = "Fat as hell!"
	where	bmi = w / h ^ 2
		(skinny, normal, fat) = (18.5, 25.0, 30.0)
{-	      	skinny = 18.5
		normal = 25.0
		fat = 30.0
-}

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
	where	(f:_) = firstname
		(l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
	where bmi wt ht = wt / ht ^ 2

-- let bindings are local and where bindings are global 
-- let <bindings> in <expression> 
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
	let sideArea = 2 * pi * r * h
	    topArea = pi * r ^ 2
	in sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of [] -> "empty."
						[x] -> "a singleton list."
						xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "This list is " ++ what xs
	where	what [] = "empty."
	      	what [x] = "a singleton list."
		what xs = "a longer list." 

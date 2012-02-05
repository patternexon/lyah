-- Wed Feb  1 00:44:20 EST 2012
-- http://www.haskell.org/haskellwiki/99_questions/11_to_20
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

-- Solution 11
-- The trick here is to understand how to create a DataType 
-- that satisfies both the conditions

data ListItem a = Single a | Multiple Int a
	deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
	where 
	  encodeHelper (1,xs) = Single xs
	  encodeHelper (n,xs) = Multiple n xs

-- Solution 12

decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decode

decode :: ListItem a -> [a]
decode (Single x) = [x]
decode (Multiple n x) = replicate n x

-- Solution 13
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs)  
	| count == 1 = Single x : encodeDirect xs
	| otherwise  = Multiple count x : encodeDirect rest
	where	(match, rest) = span (==x) xs
		count = 1 + length match


-- Solution 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x : dupli xs


-- Solution 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli xs n = concatMap (repli' n) xs 

repli' :: Int -> a -> [a]
repli' n x
	| n == 1 = [x]
	| otherwise = [x] ++  repli' (n-1) x

-- Solution 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs 1 = []
dropEvery xs n 
	| len < n = xs
	| otherwise = first ++ dropEvery second n
	where splits = splitAt (n-1) xs 
	      first = fst splits
	      second = tail (snd splits)
	      len = length xs

-- Solution 17
split :: [a] -> Int -> ([a], [a])
split xs n 
	| count < n = (xs, [])
	| otherwise =  split' [] xs n
	where count = length xs 	

split' :: [a] -> [a] -> Int ->  ([a], [a])	
split' es fs 0 = (es, fs)
split' es (f:fs) n = split' (es++[f]) fs (n-1)


-- Solution 18
slice :: [a] -> Int -> Int -> [a]
slice xs i j = if i1 > j
		then	error "Cant slice in such a way"
		else	fst (split (snd (split xs i1)) (j-i1))
		where i1 = i-1 

-- Solution 19
rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate xs n
	| n < 0 = rotate xs (len + n)
	| otherwise = rest ++ initial
		where initial = slice xs 1 n
		      rest    = slice xs (n+1) len
		      len     = length xs
		
		

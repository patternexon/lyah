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


-- Chapter 7
-- Modules

import Data.List
import Data.Char

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
	let nlen = length needle
	in foldl 
		(\acc x -> 
			if take nlen x == needle
			then True
			else acc)
	False
	(tails haystack)

encode :: Int -> String -> String
encode shift msg = 
	let ords = map ord msg
	    shifted = map (+ shift) ords
	in map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' [] = Nothing
findKey' key (k,v):xs = if key == k
			then v
			else findKey' key xs

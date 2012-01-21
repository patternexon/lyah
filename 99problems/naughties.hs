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
elementAt :: (Num b) => [a] -> b -> a
elementAt [] _ = error "List too small"
elementAt (x:_) 1 = x
elementAt (x:xs) pos = elementAt xs (pos -1)



-- Thu Mar 15 18:42:49 EDT 2012
-- http://www.haskell.org/haskellwiki/99_questions/21_to_28
module Twenties
( insertAt
, range
, rnd_select
) where
import System.Random
import Data.List
import Tweens

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

-- Solution to Problem 23
rnd_select :: (Eq a) => [a] -> Int -> [a]
rnd_select [] _ = []
rnd_select _ 0 = []
rnd_select ys n = x : rnd_select xs (n-1)
            where (x, xs) = removeAt 3 ys
--            where (x, xs) = removeAt (rnd_index ys) ys



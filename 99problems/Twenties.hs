-- Thu Mar 15 18:42:49 EDT 2012
-- http://www.haskell.org/haskellwiki/99_questions/21_to_28
module Twenties
( insertAt
, range
, rnd_select
, rnd_select'
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
rnd_select :: (Eq a, RandomGen g) => [a] -> Int -> g -> ([a], g)
rnd_select [] _ gen = ([], gen)
rnd_select _ 0 gen = ([], gen)
rnd_select ys n gen =
    let (rnd_index, gen') = randomR (1, length ys) gen
        (x, xs) = removeAt rnd_index ys
        (xs', gen'') = rnd_select xs (n-1) gen'
    in (x : xs', gen'')

-- To run 
-- *Twenties> getStdRandom (rnd_select [1..10] 3)

-- Solution to Problem 24
rnd_select' :: RandomGen g => Int -> Int -> g -> ([Int],g)
rnd_select' 0 _ gen = ([], gen)
rnd_select' _ 0 gen = ([], gen)
rnd_select' n r gen = rnd_select [1..r] n gen
--To run
-- *Twenties> getStdRandom (rnd_select' 3 10)




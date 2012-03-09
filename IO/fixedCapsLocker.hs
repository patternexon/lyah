-- Chapter 8
-- Mon Mar  5 22:21:12 EST 2012

import Data.Char

main = do
    contents <- getContents
    putStr (map toUpper contents)

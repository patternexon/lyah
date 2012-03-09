-- Chapter 8
-- Wed Mar  7 20:16:02 EST 2012

import Data.Char

main = do
    contents <- getContents
    putStr (map toUpper contents)

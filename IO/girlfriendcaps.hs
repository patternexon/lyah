--Chapter 8
--writeFile

import System.IO
import Data.Char

main = do 
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendCaps.txt" (map toUpper contents)

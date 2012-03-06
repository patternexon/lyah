-- Chapter 8

import Data.Char

main = do
	putStrLn "Whats your firstname?"
	firstName <- getLine
	putStrLn "Whats your lastname?"
	lastName <- getLine
	let bigFirstName = map toUpper firstName
	    bigLastName = map toUpper lastName
	putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

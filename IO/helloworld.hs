-- Chapter 8
-- Input / Output

--main = putStrLn "Hello World"

main = do 
	putStrLn "What is your name?"
	name <- getLine
	putStrLn ("Hello " ++ name ++ ", you apparently rock!")

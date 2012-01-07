doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleUs' x y = doubleMe x + doubleMe y


-- first if statement
doubleSmallNumber x =	if x < 100
		      	then doubleMe x
		     	else x


-- ' just means a none lazy version of the method or
--  slightly different     
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1


-- showing off the '
-- and poor taste in standup
conanO'Brien = "It's a-me, Conan O'Brien!"


-- bangBoom
boomBang xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

-- my own length function
length' xs = sum [ 1 | _ <- xs]

removeNonUpperCase xs = [ c | c <-xs, c `elem` ['A'..'Z']]

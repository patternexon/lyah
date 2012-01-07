doubleMe x = x + x

--doubleUs x y = x*2 + y*2
doubleUs x y = doubleMe x + doubleMe y


-- first if statement
doubleSmallNumber x =	if x < 100
		      	then doubleMe x
		     	else x


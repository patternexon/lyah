
module Cuboid
(	volume,
	area
) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectangleArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = 2 * rectangleArea a b +
		   2 * rectangleArea a c +
		   2 * rectangleArea c b

-- Helper method not exported
rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b


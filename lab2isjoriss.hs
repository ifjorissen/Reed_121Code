module Lab2 where
 
-- Isabella Jorissen
-- isjoriss
-- Math 121 1:00pm Lab 2 (Section 3)
-- 1:10 pm lecture MWF

--factorial takes an int n and computes n!
factorial :: Int -> Int
factorial x | x == 1    = x
            | otherwise = x * (factorial (x-1))

--Takes two integers, m and n, and computes the sum of the integers between them (and including integers m and n)
sumFromTo :: Int-> Int-> Int
sumFromTo m n | n == m   = m
              |otherwise = (sumFromTo m (n-1)) + n

--takes two integers, x and y, and computes x^y
power :: Int -> Int -> Int
power x y | y == 0    = 1 	
	  | otherwise = (power x (y-1)) * x 

-- intMod takes two integers, x and y, and uses repeated subtraction to compute x%y
intMod :: Int -> Int -> Int
intMod x y | x < y     = x
	   | otherwise =   (intMod (x-y) y)

-- intDiv takes two integers a and y and uses repeated subtraction to compute x/y
intDiv:: Int-> Int-> Int
intDiv x y | x < y     = 0
           | otherwise = (intDiv (x-y) y) + 1 
           
-- intDivMod takes two integers x and y and returns x/y and x%y in a tuple type (Int,Int)
-- I feel like this is so close!
--intDivMod :: Int-> Int -> (Int, Int)
--intDivMod x y | x < y   = (x , 0)
--            | otherwise = (fst ((intDivMod(x-y) y + 1)), snd ((intDivMod (x-y) y + 1)))
	          
--exGcd computes the greatest common denominator between two integers, x and y
exGcd :: Int-> Int->Int
exGcd x y | x==y      = y 
		  | x < y     =  exGcd(y-x) x 
		  | otherwise = exGcd (x-y) y
	
-- sumOfDigits takes a number and adds up all the digits in that number	
sumOfDigits :: Int -> Int
sumOfDigits x | x < 10    = x
	      | otherwise = sumOfDigits(x`div`10) + x`mod`10
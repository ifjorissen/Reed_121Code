module Lab1 where



--tensDigit takes an integer, x, computes the tens digit of that integer, and returns it.

tensDigit:: Int -> Int

tensDigit x = (x `div` 10) `mod` 10 


--intOftwoDigits takes in two integers, x and y, and turns them into one integer that
--is the decimal representation of those two integers, it then returns one integer.

intOfTwoDigits :: Int->Int->Int

intOfTwoDigits x y = x*10 + y

--intWithDigitsReversed takes one int and reverses the digits of the int and returns the reversed int.

intWithDigitsReversed:: Int-> Int

intWithDigitsReversed x = (x `mod` 10)*10 + x`div`10

-- subStringFromTo takes a string and two ints and returns the inputted string from the first
-- into to the second int inclusive 

subStringFromTo :: [Char] -> Int -> Int -> [Char]

subStringFromTo charList y z = take (z-1) (drop (y-1) charList) 	 

--Takes an integer, n, and computes (-1)^n

--minusOneToThe:: Int->Int (this works but i think the other solution is what you were looking for…)
--minusOneToThe n = (-1)^n


minusOneToThe :: Int-> Int
minusOneToThe n | n`mod`2 == 0 = 1
   	        | otherwise    = (-1)


--takes a list of integers and returns the middle element

middle :: [Int] -> [Int]

middle list  | ((length list) `mod` 2 == 1) = head (drop ((length list) `div` 2) list):[]
    	     | otherwise		    = drop (length list) list

--mimics the piecewise function on the wikipedia website
wikipedia :: Int -> Int

wikipedia x | x <= (-3)               = (-x) - 3
 	    | ((-3) <= x) && (x <= 0) = x + 3
	    | (0 <= x) && (x <= 3)    = (-2 * x) + 3
	    |x >= 3 		      = x - 6


--takes three integers as the coefficients of the equation ax^2 + bx + c  and returns to 
--the user the number of solutions to the equation.

quadSolns :: Int->Int->Int->Int

quadSolns a b c | (b^2) - (4*a*c) <  0 = 0
		| (b^2) - (4*a*c) == 0 = 1
		| (b^2) - (4*a*c) >  0 = 2

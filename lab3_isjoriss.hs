module Lab3 where

--Isabella Jorissen
--isjoriss
--Math 121 lab 3
--1:10 pm lecture Lab section 3

--takes an integer, x, and returns a list of the squares from x to 1. 
squaresTo :: Int -> [Int]
squaresTo x | x == 1    = [1]
            | otherwise = (x*x) : (squaresTo (x-1))
                  
--squaresToo takes an int and returns a list of the squares between 1 and x.
--uses a supplementary function, squareProcess, to create the list in the right order.
squaresToo:: Int -> [Int]
squareProcess :: Int->[Int]->[Int]
squaresToo x = squareProcess x []
squareProcess x list | x==1      = 1:(list)
					 | otherwise = squareProcess (x-1) ((x*x):(list))
           
--takes a list of integers, and returns that list of integers%2 in a list 
--takes the first element in a list, computes int%2, and then prepends the result to the remainder of the function.
parity :: [Int]->[Int]
parity x | x == []   = []
		 | otherwise = ((head x)`mod`2): (parity (tail x))
             
--determines whether or not a given integer is in a given list.  Returns a boolean.
-- "travels" through the list checking to see if the first element is equal to the given number
-- if yes, returns true, if no, checks the remainder of the list until the list is empty, then returns false.
isMemberOf :: Int -> [Int] -> Bool
isMemberOf num list | list == []       = False
	                | head list == num = True
	                | otherwise        = isMemberOf num (tail list)

--count takes an integer and a list of integers and computes how many times that integer
--appears in that list.  Returns an int.
count :: Int -> [Int] -> Int
count int list | list == [] = 0
	                | int == (head list) = 1 + count int (tail list)
	                | otherwise = count int (tail list)
	             
-- esrever reverses a list of integers and returns the reversed list
esrever :: [Int] -> [Int] 
reverseProcess :: [Int]->[Int]->[Int]
esrever list = reverseProcess list []
reverseProcess list revList | list == [] = revList
							| otherwise = reverseProcess (tail list) ((head list): (revList))
			
--takes two integers and a list and adds that integer the spot in the list as designated by the first number
-- "walks" through the list until the place requested is the first place, inserts the number, and returns the resulting list.
insertAt :: Int -> Int -> [Int] -> [Int]
insertAt place num list | place == 1 = num:list
						| otherwise  = (head list) : (insertAt (place-1) num (tail list))
								
--takes a number and returns a list of the digits in order from least significant to most significant
digitsOf :: Int -> [Int]
digitsOf num | num == 0  = []
		     | otherwise = (num`mod`10): digitsOf(num`div`10)
		  
--takes a list of integers and returns the number composed by the list 
intOfDigits :: [Int] -> Int
intOfDigits list | list == [] = 0
				 | otherwise = ((head list) * 10^((length list)-1)) + intOfDigits(tail list)
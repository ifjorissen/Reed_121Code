

module Lab4 where

--Isabella Jorissen
--Math 121 Lab 4 1:00pm Section 3
-- 1:10 Lecture MWF

--the mapOf function takes a function (that takes an int and returns and int) and 
-- also takes a list.  mapOf then returns a list with the function applied to the given list.
mapOf :: (Int -> Int) -> [Int] -> [Int]
mapOf function list | list == [] = []
				    | otherwise  = function (head list) : (mapOf function (tail list))
				    
--foldOvah takes an operation (a function that takes two ints and returns one int) and 
-- applies that operation over a given list and returns the result (an int).
foldOvah :: (Int -> Int ->Int) -> Int -> [Int] -> Int
foldOvah op base list | list == [] = base
					  | otherwise  = op (head list) (foldOvah op base (tail list))
						   
-- keepOnly takes a function (int->bool) and a list of integers and keeps only the
-- integers that satisfy the boolean function. It returns the valid integers in a list.

keepOnly :: (Int-> Bool) -> [Int] -> [Int]
keepOnly gate list | list             == []   = []
				   | gate (head list) == True = (head list) : (keepOnly gate (tail list))
				   | otherwise                = keepOnly gate (tail list)

-- lengthCount helps the count function by taking a list and returning its length using recursion
lengthCount :: [Int] -> Int
lengthCount list | null list = 0
				 | otherwise = 1 + (lengthCount (tail list)) 
				 
-- count uses lengthCount and keepOnly to determine the number of times a given integer appears in a list
-- count takes in an integer and a list and uses recursion to do this.  It returns an Int.
count :: Int -> [Int] -> Int
count x list  = lengthCount (keepOnly (==x) list)


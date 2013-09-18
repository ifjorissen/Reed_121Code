module Lab4 where

-- Isabella Jorissen
-- Lab 4 Math121 Section 4
-- 1:10 pm Lecture MWF

-- crossHelp aids cross by taking an int and a list of ints and returns a list of (Int, Int) tuples.
-- this generates one part of the cross function
crossHelp :: Int -> [Int] -> [(Int, Int)]
crossHelp num list | null list = []
				   | otherwise = (num, head list) : crossHelp num (tail list)

-- cross takes two lists of integers and returns a list of (Int, Int) tuples.  
-- the results is a list of all possible pairs between the two lists
-- cross uses cross help to generate the pairs for the first element in the first list
-- and the the second list, and then uses recursion to do the same with the remainder of the first list
cross :: [Int] -> [Int] -> [(Int, Int)]
cross listA listB | null listA = []
                  | otherwise  = (crossHelp (head listA) (listB)) ++ (cross (tail listA) listB)

--bins is a function that takes an integer and uses putBit to generate a list of a list of 
-- all the binary sequences of the length of the given integer.  
-- bins calls putBit to put a 0 in front of the first 2^(n-1) lists and a 1 in the second 2^(n-1) elements.
bins :: Int -> [[Int]]
bins num | num == 0  = [[]]
         | num == 1  = [[0],[1]]
         | otherwise = (putBit 0 (bins (num-1))) ++ (putBit 1 (bins (num-1)))

-- putBit takes an integer (0 or 1) and prepends it to the other thing it takes, [[Int]].
putBit :: Int -> [[Int]] -> [[Int]]
putBit num dbList | null dbList = []
                  | otherwise   = (num: (head dbList)) : (putBit num (tail dbList))
                  
-- like cross and crossHelp, twoListsOf takes a list and computes all the possible combinations
-- of length two in that list.  TwoListOf starts by calling twoHelp and gives it the head of the list
--and the remaining elements of the list.  
twoListsOf :: [Int] -> [[Int]]
twoListsOf list | null list = []
				| otherwise = (twoHelp (head list) (tail list)) ++ ( twoListsOf (tail list))
				
-- functions almost identically to cross help, except it returns a [[Int]] instead of a 
-- list of (Int, Int) tuples.		
twoHelp :: Int -> [Int] -> [[Int]]
twoHelp x nums | null nums = []
               | otherwise =  (x: [head nums]) : twoHelp x (tail nums)
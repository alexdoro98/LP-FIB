
import Data.List


myLength :: [Int] -> Int
-- Base Case
myLength [] = 0
-- Recursive Case
myLength a = 1 + myLength (tail a)



myMaximum :: [Int] -> Int
-- Base Case
myMaximum [a] = a
-- Recursive case
myMaximum a = if head a > myMaximum (tail a)
		then head a
		else myMaximum (tail a)



average :: [Int] -> Float
average a =  (fromIntegral (sum a)) / (fromIntegral(myLength a))



buildPalindrome :: [Int] -> [Int]
buildPalindrome a = concat [reverse a, a]



flatten :: [[Int]] -> [Int]
flatten a = concat a
--Sol:
--flatten (x:xs) = x ++ flatten xs


oddsNevens :: [Int] -> ([Int],[Int])
--Base Case: emplty list
oddsNevens [] = ([],[])
--Recusive Case
oddsNevens a = 
	if rem (head a) 2 == 0

		then ((getOdd (tail a)), 
			concat [[head a],(getEvens (tail a))] )

		else (concat [[head a],(getOdd (tail a))], 
			(getEvens (tail a)) )

getEvens :: [Int] -> [Int]
--Base Case: emplty list
getEvens [] = []
--Recursive Case:
	--if the 1st number of the vector is even we join it with the other evens calling the function
	--otherwise we call the function with the tail of the vector
getEvens a =
	if rem (head a) 2 == 0
		then concat [[head a],getEvens(tail a)]
		else getEvens(tail a)

getOdd :: [Int] -> [Int]
--Base Case: emplty list
getOdd [] = []
--Recursive Case: idem getEvens with odds
getOdd a =
	if rem (head a) 2 == 1
		then concat [[head a],getOdd(tail a)]
		else getOdd(tail a)


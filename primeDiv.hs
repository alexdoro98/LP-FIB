
--primeDivisors :: Int -> [Int]
--primeDivisors 0 = []
--primeDivisors a = 


nextPrimeDivisor :: Integer -> Integer -> Integer
nextPrimeDivisor a b = 
	if b < a && isPrime b && rem a b == 0 
		then b
		else nextPrimeDivisor a (b + 1)

isPrime :: Integer -> Bool
isPrime a = if rem (myFactorial(a - 1) + 1) a == 0 
	then True 
	else False

myFactorial :: Integer -> Integer 
myFactorial 0 = 1
myFactorial n = n * myFactorial (n - 1)


allPrimeDivisor :: Integer -> [Integer] -> [Integer]
allPrimeDivisor a b = 
	if last b * last b + 1 >= a
		then b
		else allPrimeDivisor a concat b [nextPrimeDivisor a last b]





remove :: [Int] -> [Int] -> [Int]
--Base Case: empty deleting list
remove l [] = l
--Recursive Case:
	--Delete all element of L equals to the first number of delList
	--Call to function without that number in the delList
remove l delList = 
	remove (removeElem l (head delList)) (tail delList)

removeElem :: [Int] -> Int -> [Int]

removeElem l delElem =
	if find (== delElem) l ==  Nothing
		then l
		else removeElem (delete l (find (== delElem) l) ) delElem





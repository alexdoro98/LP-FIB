
absValue :: Int -> Int
absValue a = if a < 0 then a * (-1) else a

power :: Integer -> Integer -> Integer
power a 0 = 1
power a b = 
	if (rem b 2) == 0
	then power a (div b 2) * power a (div b 2)
	else a * power a (div b 2) * power a (div b 2)

isPrime :: Integer -> Bool
isPrime a = if rem (myFactorial(a - 1) + 1) a == 0 
	then True 
	else False


--correcció
-- isPrime :: Integer -> Bool
-- isPrime x = notElem

myFactorial :: Integer -> Integer 
myFactorial 0 = 1
myFactorial n = n * myFactorial (n - 1)

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib(n - 1) + slowFib(n - 2)

-- The main idea of this algorithm is; we only need the last two values of
-- the seccesion to find the number
-- Base Case:
quickFib :: Int -> Int
quickFib n = if n < 2 
	then n 
	else quickFib_hard n 0 1

-- Recursive Case:
quickFib_hard :: Int -> Int -> Int -> Int
quickFib_hard 0 a _ = a
quickFib_hard n a b = quickFib_hard (n - 1) b (a + b) 


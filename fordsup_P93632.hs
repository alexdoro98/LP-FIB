
import Data.List


myLength :: [Int] -> Int
-- Base Case
myLength [] = 0
-- Recursive Case
myLength a = 1 + myLength (tail a)

--Feu una funció eql :: [Int] -> [Int] -> Bool 
--que indiqui si dues llistes d’enters són iguals.
eql :: [Int] -> [Int] -> Bool
eql a b = 
	myLength a == myLength b
	&&
	(all (== True) (zipWith (==) a b))

--Feu una funció prod :: [Int] -> Int 
--que calculi el producte dels elements d’una llista d’enters.
prod :: [Int] -> Int
prod a = 
	let aProd = scanl (*) 1 a
	in last aProd

--Feu una funció prodOfEvens :: [Int] -> Int 
--que multiplica tots el nombres parells d’una llista d’enters.
prodOfEvens :: [Int] -> Int
prodOfEvens a = 
	let aEvens = filter even a
	in prod aEvens

--Feu una funció powersOf2 :: [Int] 
--que generi la llista de totes les potències de 2.
powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

--Feu una funció scalarProduct :: [Float] -> [Float] -> Float 
--que calculi el producte escalar de dues llistes de reals de la mateixa mida.
scalarProduct :: [Float] -> [Float] -> Float
scalarProduct a b =
	let ab = zipWith (*) a b
	in sum ab

--Tambe podria ser
-- scalarProduct :: [Float] -> [Float] -> Float
-- scalarProduct a b = sum $ zipWith (*) a b
	
























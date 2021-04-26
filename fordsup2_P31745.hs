
flatten :: [[Int]] -> [Int]
flatten a = foldl (++) [] a

--Feu una funció myLength :: String -> Int 
--que retorna la llargada d’una cadena de caràcters.
myLength :: String -> Int
myLength a = foldl (+) 0 $ map (const 1) a 
--  myLength = sum . map (const 1)
-- el . concatena dues funcions

-- Feu una funció myReverse :: [Int] -> [Int] 
-- que inverteix els elements d’una llista d’enters.
myReverse :: [Int] -> [Int]
myReverse = foldl (flip(:)) []
-- els ':' actuen com un "la llista que introdueixis"

-- Feu una funció countIn :: [[Int]] -> Int -> [Int] 
-- que, donada una llista de llistes d’elements ℓ i un element x 
-- ens torna la llista que indica quants cops apareix x en cada llista de ℓ.
countIn :: [[Int]] -> Int -> [Int]
countIn list a = map (length . filter(== a)) list

{-
countIn xl a = map countElem xl
		where countElem list =
				sum $ map (\b -> if (a == b)
									then 1
									else 0)	list
-}

-- Feu una funció firstWord :: String -> String 
-- que, donat un string amb blancs i caràcacters alfabètics), 
-- en retorna la primera paraula.
firstWord :: String -> String
firstWord s = takeWhile(/= ' ') x
	where x = dropWhile(== ' ') s
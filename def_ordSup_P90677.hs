
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ a [] = a
myFoldl f a (l:list) = myFoldl f (f a l) list

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z [] = z
myFoldr f z (l:list) = f l $ myFoldr f z list

--myIterate :: (a -> a) -> a -> [a]
--myIterate f a = a : myIterate( f $ f a)
)
myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil b f a =
	if b a then
		a
	else
		myUntil b f $ f a

myMap :: (a -> b) -> [a] -> [b]
myMap f list = myFoldr (\b auxList -> f b : auxList  ) [] list

--Funcionament del MyFilter:
-- Al utilitzar el foldr l'ultim element (per l'esquerra) s'utilitza al
-- final, no al principi, per aixo, anira fent tots els if/else i al final
-- ho juntara tot    
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter b list = foldr(\a auxList ->   if b a then a:auxList
                                        else auxList ) [] list
myAll :: (a -> Bool) -> [a] -> Bool
myAll cond list = and (map cond list)

myAny :: (a -> Bool) -> [a] -> Bool
myAny cond list = or (map cond list)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (a:listA) (b:listB) = (a,b):myZip listA listB

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f listA listB = map g $ zip listA listB
                    where g (a,b) = f a b

--myZipWith f [] _ = []
--myZipWith f _ [] = []
--myZipWith f (a:listA) (b:listB) = (f a b):myZipWith f listA listB
                                










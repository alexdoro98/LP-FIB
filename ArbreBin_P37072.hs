
data Tree a = Node a (Tree a) (Tree a) | Empty 
	deriving (Show)

--size :: Tree a -> Int que, donat un arbre, retorni la seva talla, és a dir, el nombre de nodes que conté.
size :: Tree a -> Int
size Empty = 0
size (Node _ aLeft aRight) = 1 + size aLeft + size aRight

--height :: Tree a -> Int 
--donat un arbre, retorni la seva alçada, 
--assumint que els arbres buits tenen alçada zero.
height :: Tree a -> Int
height Empty = 0
height (Node _ aLeft aRight) = 1 + max (height aLeft) (height aRight)

--equal :: Eq a => Tree a -> Tree a -> Bool 
--donat dos arbres, indiqui si són el mateix.
equal :: Eq a => Tree a -> Tree a -> Bool 
equal Empty Empty = True
equal Empty (Node _ _ _) = False
equal (Node _ _ _) Empty = False
equal (Node a aLeft aRight) (Node b bLeft bRight) = 
		(a == b) && (equal aLeft bLeft) && (equal aRight bRight)

--isomorphic :: Eq a => Tree a -> Tree a -> Bool 
--donat un arbres, indiqui si són el isomorfs,
--si es pot obtenir l’un de l’altre tot girant algun dels seus fills.

isomorphic :: Eq a => Tree a -> Tree a -> Bool 
isomorphic Empty Empty = True
isomorphic Empty (Node _ _ _) = False
isomorphic (Node _ _ _) Empty = False
isomorphic (Node a aLeft aRight) (Node b bLeft bRight) =
	(a == b) &&
	( 	(equal aLeft bLeft) && (equal aRight bRight) ||
		(equal aLeft bRight) && (equal aRight bLeft) 	)

--preOrder :: Tree a -> [a]
--donat un arbre, retorni el seu recorregut en pre-ordre.
preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a aLeft aRight) = [a] ++ preOrder aLeft ++ preOrder aRight

--postOrder :: Tree a -> [a]
--donat un arbre, retorni el seu recorregut en post-ordre.
postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a aLeft aRight) = postOrder aLeft ++ postOrder aRight ++ [a]

--inOrder :: Tree a -> [a] 
--donat un arbre, retorni el seu recorregut en in-ordre.
inOrder :: Tree a -> [a] 
inOrder Empty = []
inOrder (Node a aLeft aRight) = inOrder aLeft ++ [a] ++  inOrder aRight 

--breadthFirst :: Tree a -> [a] 
--donat un arbre, retorni el seu recorregut per nivells.
breadthFirst :: Tree a -> [a] 
breadthFirst a = breadthFirstRec [a]

breadthFirstRec :: [Tree a] -> [a] 
breadthFirstRec [] = []
breadthFirstRec (Empty : cua) = breadthFirstRec cua
breadthFirstRec ( (Node a aLeft aRight) : cua ) = [a] ++ (breadthFirstRec (cua ++ [aLeft, aRight]))

--build :: Eq a => [a] -> [a] -> Tree a 
--donat el recorregut en pre-ordre d’un arbre i el recorregut en in-ordre del 
--mateix arbre, retorni l’arbre original. Assumiu que l’arbre no té elements repetits.
build :: Eq a => [a] -> [a] -> Tree a 
build [] [] = Empty
build (a:preOrd) inOrd = Node a aLeft aRight
	where 
		aLeft 	= build preOrd1 inOrd1
		aRight 	= build preOrd2 inOrd2
		(inOrd1, inOrd2)	= split a inOrd 				
		(preOrd1, preOrd2) 	= splitAt (length inOrd1) preOrd

-- split x (a1,...,an,x,b1,...,bm) returns (a1,...an, b1...bm)
split:: Eq a => a -> [a] -> ([a],[a])
split x xs = (p1, tail p2)
	where
		(p1, p2) = splitAt (indexOf xs) xs 
		indexOf(y:ys)
			|x==y 		= 0
			|otherwise	= 1 + indexOf ys

--overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree 
--donats dos arbres, retorni la seva superposició utilitzant una funció.
--Superposar dos arbres amb una funció consisteix en posar els dos arbres 
--l’un damunt de l’altre i combinar els nodes doble resultants amb la funció 
--donada o deixant els nodes simples tal qual.
overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty Empty = Empty
overlap _ a Empty = a
overlap _ Empty a = a
overlap f (Node a aLeft aRight) (Node b bLeft bRight) =
	(Node (f a b) (overlap f aLeft bLeft) (overlap f aRight bRight))





















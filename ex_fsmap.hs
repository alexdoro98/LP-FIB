{-
P48366 Problema 3 (Parcial 2017-12-04)
Definiu una funció fsmap :: a -> [a -> a] -> a que, 
donats un element x de tipus a i una llista fs de funcions de
tipus a -> a, fa que fsmap x fs retorni l’aplicació (d’esquerra a dreta) 
de totes les funcions de fs a x. Es valorà
com de succinta és la vostra solució.

fsmap 3 [(+2), (*3), (+4)] ===> 19
fsmap "o" [(++"la"), (:)'h', (++"!")] ===> "hola!"
fsmap False [] ===> False
-}

fsmap :: a -> [a -> a] -> a
fsmap = foldl $ flip ($)
-- fsmap x fs = (foldl ( flip (.)) id fs) x


{-
selectFirst	P91910 Apartat 2 (Parcial 2018-04-11)
Escriviu una funció selectFirst :: [Int] -> [Int] -> [Int] -> [Int] que, donades tres 
llistes l1, l2 i l3 retona
els elements de l1 que apareixen a l2 en una posició menor estrictament que a l3. 
Si un element apareix a l2 i no a l3 es considera que apareix en una posició anterior.

selectFirst [] [] [] ===> []
selectFirst [8,4,5,6,12,1] [] [8,6,5,4,1] ===> []
selectFirst [8,4,5,6,12,1] [4,5,6,2,8,12] [] ===> [8,4,5,6,12]
selectFirst [8,4,5,6,12,1] [4,5,6,2,8,12] [8,6,5,4,1] ===> [4,5,12]
-}

selectFirst :: [Int] -> [Int] -> [Int] -> [Int
selectFirst a [] c = []
selectFirst a b c = filter ofirst a
	where
		nB = lenght b
		nC = lenght c
		ofirst x = 		let tB = lenght (takeWhile (/=x) b) // tB es la distància del inici...x
							tC = lenght (takeWhile (/=x) c)	// idem

						in 	

{-
show		P24239 Apartat 5 (Parcial 2018-11-06)
Considereu el següent tipus genèric LTree a d’arbres binaris amb valors a les fulles:
   data LTree a = Leaf a | Node (LTree a) (LTree a)
Feu que els arbres siguin instàncies de la classe Show visualitzant-se segons els exemples.
Node (Leaf 3) (Node (Leaf 8) (Leaf 7)) ===> <{3},<{8},{7}>>
Node (Leaf 1) (Node (Node (Leaf 3) (Leaf 4)) (Node (Leaf 8) (Leaf 7))) ===> <{1},<<{3},{4}>,<{8},{7}>>>
Node (Leaf "Albert") (Node (Leaf "Gerard") (Leaf "Jordi")) ===> <{"Albert"},<{"Gerard"},{"Jordi"}>>
Leaf 'x' ===> {'x'}
-}

data LTree a = Leaf a | Node (LTree a) (LTree a) 
instance Show a => Show (LTree a)
    where
        show (Leaf x) = "{" ++ show x ++ "}"
        show (Node l r) = "<" ++ show l ++ "," ++ show r ++ ">"





























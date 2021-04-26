{-# LANGUAGE RecordWildCards #-}    -- per utilitzar fields

--import System.IO
import Data.Char (isUpper)
import Data.List (nub, isInfixOf)
import Data.List.Split (splitOn)    -- cal instal·lar: cabal install split
import Data.String.Utils (strip)    -- cal instal·lar: cabal install MissingH
import Data.Maybe (mapMaybe, fromMaybe)


type Programa = [ Regla ]

data Regla = Regla { _cap::Atom, _cos::[ Atom ] } deriving (Eq, Show)

type Sustitucio = [ (Term, Term) ]     -- [(variable, constant), (variable, constant), ...]

type BaseConeixement = [ Atom ]

data Term = 
	Var String 
	| Sym String
   deriving (Eq, Show)

data Atom = Atom { _nomPredicat::String, _termes::[ Term ] }
    deriving (Eq, Show)


--FUNCIONS PER LA CREACIÓ DE LA BaseConeixement INICIAL
--------------------------------------------------------

-- Input: 	String 
-- Output: 	Bool
-- Si l'String que entra contè els caracters "query" retornem cert ja que es una query
isQuery :: String -> Bool
isQuery = isInfixOf "query"

-- Input: 	String 
-- Output: 	Bool
-- Si l'String que entra contè els caracters "=>" retornem cert ja que es una regla 
-- i abans de cridar aquesta funció ja haurem comprobat que no es una query
isRegla :: String -> Bool
isRegla = isInfixOf "=>"


--Input:  	String & [String]
--Output:  ([String], [String])
-- Divideix la llista d'strings en dos llistes d'String separades per el string 
-- cond sense estar contingut en cap de les dues llistes
mySplitAt :: String -> [String] -> ([String],[String])
mySplitAt cond (l:list) 
	| cond == l  	= ([],(list))
	| otherwise 	= (l:a, b)
	where (a, b) = mySplitAt cond list

--Input: 	String
--Output:	Term
-- 	Si la primera lletra de s és majúscula és un Term, Var; en cas contrari Sym
createTerm ::String -> Term
createTerm s =
	if isUpper (s !! 0) 
		then Var s
	else
		Sym s

--Input: 	[String] 
--Output:	[Term]
-- Agafa una llista d'String i la converteix en una llista de Terms de manera recusiva
makeTerms :: [String] -> [Term]
makeTerms [x] = [createTerm x]
makeTerms (t:terms) = [createTerm t] ++ makeTerms terms

--Input:    [String] 
--Output:	[Atoms]
-- Agafa el primer element de la llista i la utilitza com a _nomPredicat de l'àtom, i amb els altres termes construeix els _termes
makeAtom :: [String] -> Atom
makeAtom (name:terms) = 
	Atom name (makeTerms terms)

--Input: 	1. un sol àtom en forma de arry de strings
--			2. un conjunt d'àtoms separats per &
--Output:	Un arry d'atoms
-- Aquesta funció serveix per contruïr els atoms de una regla amb més d'un àtoma al cos
makeAtomsFromCos :: [String] -> [Atom]
makeAtomsFromCos s 
	| elem "&" s 	=	[makeAtom a] ++ (makeAtomsFromCos b)
	| otherwise 	=	[makeAtom s]
		where (a,b) = mySplitAt "&" s

-- Input:	[String]
-- Output:  (Atom, [Atom])
-- dividim la llista d'Strings per la flexa que denota la difernecia entre el cap i el cos d'una regla
divideCapCos :: [String] -> (Atom, [Atom])
divideCapCos line = (makeAtom cap, makeAtomsFromCos cos)
	where (cos, cap) = mySplitAt "=>" line

-- Input: 	[String] 
-- Output: 	Regla
-- Creem una regla apartir de la definció de Regla ajudant-nos d'alguna variable auxiliar
makeRegla :: [String] -> Regla
makeRegla line =
	Regla cap cos
		where (cap, cos) = divideCapCos line

-- Input: 	String 
-- Output: 	Regla
-- Depenen de quins caracters hi hagi dins del string sabem quin tipus de regla hem de crear
parsePallus :: String -> Regla
parsePallus s 
	| isQuery s	= makeRegla $ words s
	| isRegla s	= makeRegla	$ words s
	| otherwise	= Regla (makeAtom $ words s) []

-- Input: 	[String] 
-- Output: 	Programa
-- Ens donen un conjunt d'Strings i retornem un conjunt de regles, cada línea del Strings es una regla
crateProgram :: [String] -> Programa
crateProgram [] = []
crateProgram (l:lines) = 
	[parsePallus l] ++ crateProgram lines


-- FUNCIONS PER LA CRECIÓ I TRACTAMENT DE SUSTITUCIONS
------------------------------------------------------

-- Input: 	Term
-- Output: 	Bool
-- En cas que el Term sigui Sym retorna True, en cas contrari retorna False
isSym :: Term -> Bool
isSym (Sym s) = True
isSym (Var s) = False

-- Input: 	[ Term ] & [ Term ] 
-- Output: 	Sustitucio
-- Fusiona dues llistes de termes en una sustitució.
mergeTerms :: [ Term ] -> [ Term ] -> Sustitucio
mergeTerms [] [] = []
mergeTerms (t1:terms1) (t2:terms2) 
	| t1 /= t2 	=	(t1, t2):(mergeTerms terms1 terms2)
	| otherwise	=	mergeTerms terms1 terms2

-- InPut: 	[ Term ] & [ Term ] 
-- OutPut: 	Sustitucio
-- Funcio utilitzada com a llençadora de la anterior per comprovar que els dos termes tinguin
-- la mateixa llargada 
generateSustitucio :: [ Term ] -> [ Term ] -> Sustitucio
generateSustitucio terms1 terms2
	| length terms1 /= length terms2 	= []
	| otherwise							= mergeTerms terms1 terms2

-- Input:	(Term,Term), [Term]
-- Output: 	[Term]
-- Aquesta funcio s'utilitza amb el suposit que [Term] es el cos d'un atom 
-- que es al cual se li ha d'aplicar la subsitució (Term, Term). Retorna
-- els termes de l'àtom modificat
makeSustitucio :: (Term, Term) -> [Term] -> [Term]
makeSustitucio _ [] = []
makeSustitucio (var,sym) (t:terms)
	| var == t 	= (sym:terms)
	| otherwise = (t: (makeSustitucio (var,sym) terms) )

-- Input:	(Term,Term) & Sustitucio
-- Output: 	Bool
-- Comprovem que no hi hagi cap contradicció entre una parella (Term,Term) i la resta
-- de les sustitucions
findContradiction :: (Term,Term) -> Sustitucio -> Bool
findContradiction s [] 
	|	isSym var						= True
	|	otherwise						= False
	where (var,sym) = s
findContradiction (varT,symT) (s:subst)
	|	isSym varT						= True
	|  	varT == varS					= True
	|	otherwise 						= findContradiction (varT,symT) subst
		where (varS,symS) = s

-- Input:	Sustitucio
-- Output:	Sustitucio
-- Aquesta funció s'encarrega de revisar una Sustitusio, si troba que algun
-- element produeix contradicció retorna una Sustitucio buida
revisarSubstitucions :: Sustitucio -> Sustitucio
revisarSubstitucions [] = []
revisarSubstitucions (s:subst)
	| findContradiction (var,sym) subst 	= []
	| otherwise								= (s:(revisarSubstitucions subst))
		where (var,sym) = s

-- Input: 	Sustitucio & [ Sustitucio ] 
-- Output: 	[ Sustitucio ]
-- Aquesta funció serveix per afegir la sustitució que
-- ha "provocat" la llista de sustitucions creades a partir d'ella
unirSustitucioMareSustitucionsFilles :: Sustitucio -> [ Sustitucio ] -> [ Sustitucio ]
unirSustitucioMareSustitucionsFilles s0 [] = []
unirSustitucioMareSustitucionsFilles s0 (s:sFilles) 
	| length s == 0 = [] ++ unirSustitucioMareSustitucionsFilles s0 sFilles
	| otherwise 	= [(s0 ++ s)] ++ unirSustitucioMareSustitucionsFilles s0 sFilles


-- FUNCIONS PEL TRACTAMENT D'ATOMS
-----------------------------------------

-- Input: 	Atom & Sustitucio
-- Output: 	Bool
-- Aquesta funcio comprova que la sustitucio es pot fer sobre l'atom
-- simplement comprova que alguna de les Vars de la sustitució formi part dels
-- termes de l'àtom.
isSustituible :: Atom -> Sustitucio -> Bool
isSustituible at [] 	= False
isSustituible at (terms:s)
	| isInfixOf [var] (_termes at) 	= True
	| otherwise						= isSustituible at s
	where
		(var,sym) = terms

-- Input: 	Atom
-- Output: 	Bool
-- Retorna cert si l'Atom es un fet, 
-- 	A cada iteració mirem si el primer terme de l'àtom és Sym o Var
-- 	En cas que sigui Var, retornem falç, en cas contrari ferm una crida recursiva
-- 	Si arribem al punt que no queden termes a analitzar voldrà dir que l'àtom es un fet.
isAtomFact :: Atom -> Bool
isAtomFact a 
	| length (_termes a) == 0 			= True 
	| not $ isSym (head(_termes a))		= False
	| otherwise							= isAtomFact (Atom (_nomPredicat a) (tail $ (_termes a)))

-- Input: 	Atom & Sustitucio
-- Output: 	Atom
-- Si el primer (Term,Term) sustitució es buida es retorna el mateix atom, 
-- en cas contrari s'aplica la primera parella (Term,Term) a l'Atom i es fa 
-- una crida recursiva per cridar als demés.
sustitueix :: Atom -> Sustitucio -> Atom
sustitueix at [s]		= Atom (_nomPredicat at) (makeSustitucio s (_termes at) )
sustitueix at (s:sus) 
	| length s == 0		= at
	| otherwise 		= sustitueix (Atom (_nomPredicat at) (makeSustitucio s (_termes at) )) sus

-- Input: 	BaseConeixement & Atom 
-- Output: 	[ Sustitucio ]
-- Aquesta funció agafa cada Atom de la BaseConeixement i el compara amb l'Atom d'entrada
-- si tenen el mateix _nomPredicat genera una sustitució i ho concatena amb la crida recursivament.
getAtomSustitucions :: BaseConeixement -> Atom -> [ Sustitucio ]
getAtomSustitucions [] atR = []
getAtomSustitucions (at:bc) atR
	| _nomPredicat at == _nomPredicat atR  	= [revisarSubstitucions $ generateSustitucio (_termes atR) (_termes at)] ++ getAtomSustitucions bc atR
	| otherwise								= getAtomSustitucions bc atR

-- Input: 	Atom & [ Sustitucio ] & BaseConeixement 
-- Output: 	[ Sustitucio ]
-- Amb aquesta generem les Sustitucions que es generen al aplicarli a un Atom una llista de Sustitusions
-- perque el resultat sigui correcte cal despres de cridar getAtomSustitucions afegir la sustitució que  
-- ha "probocat" aquestes sustitucions, aixo ho fem amb una funció auxiliar
-- finalment concatenem la crida recurisva, cridant-la amb els demes elements de la llista.
avaluaAtom :: Atom -> [ Sustitucio ] -> BaseConeixement -> [ Sustitucio ]
avaluaAtom atR [] bc = []
avaluaAtom atR (s:sus) bc
	| isSustituible atR s	= (unirSustitucioMareSustitucionsFilles s (getAtomSustitucions bc newAtR)) ++ avaluaAtom atR sus bc
	| otherwise 			= avaluaAtom atR sus bc
	where
		newAtR = sustitueix atR s

-- FUNCIONS PEL TRACTAMENT DE RELGES
-----------------------------------------

-- Input: Regla
-- Output: Bool
-- Retorna cert si la regla és un fet, una regla és un fet si el cos de la regla és buit
-- i l'àtom del cap és un fet.
isRuleFact :: Regla -> Bool
isRuleFact r 
	| length (_cos r) == 0 && isAtomFact (_cap r) 	= True
	| otherwise										= False

-- Input: 	BaseConeixement & Regla & [ Sustitucio ]
-- Output: 	[ Sustitucio ]
-- L'entrada d'aquesta funció és:
--	BaseConeixement 	On volem aplicar la regla
--	Regla 				que volem aplicar
-- 	[ Sustitucio ] 		que són les sustitucions que sabem que podem aplicar a l'últim element
--						tractat de la regla, aquest element és el que aniria davant del primer element
--						que tenim al cos
-- En cas que el cos sigui buit retornem [], ja que ja hem tractat tota la regla,
-- en cas contrari, retornem la llista de substitucions aplicades al primer element del cos de la regla
-- afegint el que ens retorni la crida recursiva amb la regla traient-li el primer element del cos
-- i amb la llista de sustitucions del primer element del cos.
avaluaReglaComplexa :: BaseConeixement -> Regla -> [ Sustitucio ] -> [ Sustitucio ]
avaluaReglaComplexa bc r sus  
	| (_cos r) == []			 	= []
	| otherwise						= atSustitucions ++ (avaluaReglaComplexa bc newR atSustitucions)
	where
		newR = Regla (_cap r) (tail (_cos r))
		atSustitucions = avaluaAtom (head (_cos r)) sus bc

-- Input: 	Regla & [ Sustitucio ] 
-- Output: 	BaseConeixement
-- Aquesta funció agafa una regla i li aplica totes les substitucions de la llista de
-- sustitucions al cap de la regla, genera tots el atoms generables d'aquesta llista.
generateFactsFromRule :: Regla -> [ Sustitucio ] -> BaseConeixement
generateFactsFromRule r [] 		= []
generateFactsFromRule r (s:sus)
	| isAtomFact newAt  = [newAt] ++ generateFactsFromRule r sus
	| otherwise 		= generateFactsFromRule r sus
	where 
		newAt = sustitueix (_cap r) s

-- Input: 	BaseConeixement & Programa
-- Output: 	BaseConeixement
-- Aquesta funció la utilitzarem per avaluar regles, 
-- en cas de que només hi hagi un element en el cos de la regla anirem a directament a 
-- generar fets amb aquesta regla
-- en cas contrari, avaluarem la regla amb una funcio per tractar regles complexes i
-- el resultat l'utilitzarem per cridar directament la funcio per generar fets apartir de una
-- regla. 
avaluaRegla :: BaseConeixement -> Regla -> BaseConeixement
avaluaRegla bc r
	| length (_cos r) == 1 	=	generateFactsFromRule r s
	| otherwise				=	generateFactsFromRule r (avaluaReglaComplexa bc newR s)
	where 
		s = getAtomSustitucions bc (head (_cos r))
		newR = Regla (_cap r) (tail (_cos r))

-- Input: 	BaseConeixement & Programa
-- Output: 	BaseConeixement
-- SUSPOSICIÓ: Tota regla es pot aplicar a la BaseConeixement, si això no es així 
-- 				aquesta funció entrarà en un bucle infinit.
-- Aquesta funció es l'encarregada de llegir un conjunt de regles i aplicar-les
-- hi han 3 tipus de regles a tractar:
--	Fets:	Regles que simplement tenen Atoms a introduir a la base de dades
--		El que farem amb elles es afegir l'Atom a la BaseConeixement per radera i farem una
--		crida recursiva amb la BaseConeixement modificada
--	Regles aplicables: Regles que generaran nous fets i les podem aplicar en aquell moment
--						en el programa ja que existeixen antecedents unificables amb Atoms
--						existents en la BaseConeixement 
--		Si una regla es aplicables el resultat d'avaluar regla sera un atom i per tant
--		l'afegirem a la BaseConeixement per radera i farem una crida recursiva amb la BaseConeixement 
--		modificada
-- Regles no aplicables: Regles que generaran nous fets i no les podem aplicar en aquell moment
--						en el programa ja que no existeixen antecedents unificables amb Atoms
--						existents en la BaseConeixement 
--		En aquest cas afegirem la regla al programa per la part de radera per tornarla a avaluar
--		més endevant i farem una crida recursiva amb la BaseConeixement modificada.
avaluaPrograma :: BaseConeixement -> Programa -> BaseConeixement
avaluaPrograma bc [] = bc
avaluaPrograma bc (r:pr)
	| isRuleFact r 						= avaluaPrograma (bc ++ [_cap r]) pr
	| not (isRuleFact r) && avR /= []	= avaluaPrograma (bc ++ avR ) pr
	| not (isRuleFact r) && avR == []	= avaluaPrograma bc (pr ++ [r])
	| otherwise 						= avaluaPrograma bc pr
	where avR = avaluaRegla bc r

--	FUNCIONS PEL TRACTAMENT DE QUERYS
--------------------------------------

-- Input: 	BaseConeixement & Atom & [Sustitucio] 
-- Output: 	[Sustitucio]
-- Aquesta funció li arriba l'últim Atom del cos d'una query. El que fa
-- és aplicar aquest Atom totes les Sustitucions de la llista i si el resultat
-- forma part de la BaseConeixement concatena la sustitucio amb la crida recursiva
-- que analitza la resta de sustitucions.
endQuery :: BaseConeixement -> Atom -> [Sustitucio] -> [Sustitucio]
endQuery bc at [] = []
endQuery bc at (s:sus)
	| isInfixOf [newAt] bc 	= [s] ++ endQuery bc at sus
	| otherwise 			= endQuery bc at sus
	where newAt = sustitueix at s

-- Input: 	BaseConeixement & Atom & [Sustitucio] 
-- Output: 	[Sustitucio]
-- Amb aquesta funcio generem totes les sustitucions posibles de aplicar a un
-- Atom una Sustitusio de la llista.
getQuerySustitutions :: BaseConeixement -> Atom -> [Sustitucio] -> [Sustitucio]
getQuerySustitutions bc atQ [] = []
getQuerySustitutions bc atQ (s:sus)  
	| isSustituible atQ s 	= [s] ++ (getAtomSustitucions bc newAtQ) ++ getQuerySustitutions bc atQ sus
	| otherwise				= getQuerySustitutions bc atQ sus
	where newAtQ = sustitueix atQ s 

-- Input: 	BaseConeixement & Regla
-- Output: 	[Sustitucio]
-- Aquesta funció la utilitzarem per avaluar querys, en cas que només hi hagi un 
-- element en el cos de la query anirem a directament a finalitzar el tractament de la query
-- en cas contrari, generarem les sustitucions que provoquen el primer element del cos de la query les 
-- les sustitucions que es generen d'ell i el resultat el concatenarem amb la crida recursiva amb la 
-- query sense el primer element del cos.
avaluaComplexQuery :: BaseConeixement -> Regla -> [Sustitucio]
avaluaComplexQuery bc q
	| length (_cos q) == 1 	=  endQuery bc (head (_cos q)) s
	| otherwise				=  (getQuerySustitutions bc (head (_cos q)) s) ++ avaluaComplexQuery bc newQ
	where 
		s = getAtomSustitucions bc (head (_cos q))
		newQ = Regla (_cap q) (tail (_cos q))

-- Input:	BaseConeixement & Regla
-- Output:	[Sustitucio]
-- Aquesta funció simplement agafa l'input i crida a una funció correctora del resultat de
-- cridar a la funció avaluaComplexQuery.
avaluaComplexQuerys :: BaseConeixement -> Regla -> [Sustitucio]
avaluaComplexQuerys bc q = allRight bc q (avaluaComplexQuery bc q)

-- Input: 	Bool
-- Output:	Sustitucio
-- Aquesta funció tradueix de un boolea a una Sustitusio.
boolToSubs :: Bool -> Sustitucio
boolToSubs True = [(createTerm "True", createTerm "True")]
boolToSubs False = [(createTerm "False", createTerm "False")]

-- Input:	[Atom] & Sustitucio
-- Output: 	[Atom]
-- Aquesta funcio agafa un conjunt d'àtoms i els hi aplica una sustitució i retorna el resultat.
aplicaSustitucions :: [Atom] -> Sustitucio -> [Atom]
aplicaSustitucions [] s = []
aplicaSustitucions (at:atoms) s = [(sustitueix at s)] ++ (aplicaSustitucions atoms s)

-- Input: 	BaseConeixement & [Atom]
-- Output: 	Bool
-- Aquesta funcio retorna cert si tots els elements d'una llista d'Atoms esta en la BaseConeixement
-- en cas contrari retorna fals.
allinBC :: BaseConeixement -> [Atom] -> Bool
allinBC bc [] = True
allinBC bc (at:atoms)
	| not(isInfixOf [at] bc) 	= False
	| otherwise					= allinBC bc atoms

-- Input: 	BaseConeixement & Regla & [Sustitucio]
-- Output: 	[Sustitucio]
-- Aquesta funcio serveix per repasar que una llista de sustitucions generades per una
-- query es correcte. El que fa es per cada Sustitucio de la llista comprova si totes les 
-- sustitucions aplicades als Atoms del cos de la query estan dins de la BaseConeixement,
-- si es aixi conctena la Sustitucio a la crida recursiva que analitza la resta de sustitusions.
allRight :: BaseConeixement -> Regla -> [Sustitucio] -> [Sustitucio]
allRight bc q [] = []
allRight bc q (s:sus)
	| s /= [] && allinBC bc (aplicaSustitucions cosQ s) 	= [s] ++ allRight bc q sus
	| otherwise												= allRight bc q sus
	where cosQ = _cos q

-- Input: 	[Term]
-- Output: 	Bool
-- Recorre una llista de Term i si troba un que es Var retorna cert, si acaba la llista retorna fals.
haveVars :: [Term] -> Bool
haveVars [] 			= False	
haveVars (t:term)
	| not (isSym t) 	= True
	| otherwise 		= haveVars term

-- Input:	BaseConeixement & Programa
-- Output:	[Sustitucio]
-- Ens entra la base de dades i un conjunt de querys
--		-Query True/False
-- 		Si la query té només un element en el cos i no hi ha termes Var en el cos
-- 		Voldra dir que és una query que espera un resulat de true o false i per tant
-- 		mirem si el cos de la query està en la base de dades. Si està, com hem de retornar
-- 		un conjunt de Sustituicons, retornarem una Sustitucio [(True, true)], per indicar cert
-- 		i en cas contrari [(False, False)].
--		-Query amb un sol element al cos
--		Cridem a la funció getAtomSustitucions que ens genera les sustitucions que se li haurien
--		de fer a l'àtom perquè sigues igual a un àtom de la base de dades
--		-Query amb més d'un element al cos
-- 		Cridem al mètode avaluaComplexQuery que s'encarrega d'avaluar aquest tipus de querys
-- Al final del tractament de la primera regla del programam tornam a cridar aquesta funció
-- sense el element ja tractat
avaluaQuerys :: BaseConeixement -> Programa -> [Sustitucio]
avaluaQuerys bc [] = []
avaluaQuerys bc (q:querys)
	| length (_cos q) == 1 && not(haveVars termsCosQ)	= [boolToSubs $ isInfixOf [head (_cos q)] bc] ++ (avaluaQuerys bc querys)
	| length (_cos q) == 1 && haveVars termsCosQ		= noRepeticions (getAtomSustitucions bc (head (_cos q))	 ++ (avaluaQuerys bc querys))
	| otherwise 										= noRepeticions ( (avaluaComplexQuerys bc q) ++ (avaluaQuerys bc querys))
	where termsCosQ = _termes (head (_cos q))

-- Input:	[String]
-- Output:	[String]
-- Com el resultat de les querys pot venir amb repeticions eliminem els repetits
-- mirant si el primer element de la llista està la resta de la llista
noRepeticions :: [Sustitucio] -> [Sustitucio]
noRepeticions [] = []
noRepeticions (s:sus)
	| not (isInfixOf [s] sus) 	= [s] ++ noRepeticions (sus)
	| otherwise					= noRepeticions (sus)


-- FUNCIONS ENTRADA/SORITDA
--------------------------------------
-- Input:	[Char]
-- Output:	IO [Char]
-- Aquesta funció la utilitzem per llegir l'entrada primer de tots els fets i regles
-- del programa i després de totes les querys
readToEnd :: [Char] -> IO [Char]
readToEnd s = do
    l <- getLine
    if l == "end."
      then return $ s
      else readToEnd (s ++ l)

-- FUNCIÓ MAIN
--------------------------------------
-- Llegim l'entrada fins al primer "end." i generem la BaseConeixement
-- Seguidament llegim el conjunt de querys que volem consultar a la BaseConeixement
-- En cas que la query sigui:
--	True/False query 	Escriurà per pantalla [(True, True)], en cas de ser certa i
--						escriurà [(False, False)] en cas de ser falsa
-- 	En cas contrari		Escriurà una llista dels conjunts de termes que compleixen 
--						la query
-- Per desgracia no he conseguit treure els resultats per pantall de una manera maca
-- i surten tots seguits.

main :: IO ()
main = do

	content <- readToEnd ""
	let (a,b) = mySplitAt "" $ splitOn "." content
	let pr = crateProgram a
	let bc = avaluaPrograma [] pr
	
	quests <- readToEnd ""
	let (c,d) = mySplitAt "" $ splitOn "." quests
	print $ avaluaQuerys bc (crateProgram c)

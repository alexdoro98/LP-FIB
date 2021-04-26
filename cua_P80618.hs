

data Queue a = Queue [a] [a]
     deriving (Show)
 
create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push a (Queue q1 q2 ) = Queue q1 (a:q2)

pop :: Queue a -> Queue a
pop (Queue [] q2) = pop $ Queue (reverse q2) []
pop (Queue (a:q1) q2) = Queue q1 q2

top :: Queue a -> a
top (Queue [] q2) = last q2
top (Queue (a:_) _) = a

empty :: Queue a -> Bool
empty (Queue [] [])	= True
empty (Queue _ _)	= False

instance Eq a => Eq (Queue a) where
	Queue a1 a2 == Queue b1 b2 
		= a1 ++ (reverse a2) == b1 ++ (reverse b2)
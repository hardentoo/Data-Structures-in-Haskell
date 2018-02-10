class Diccionario t where
	vacia    :: Ord k => t k v
	insertar :: Ord k => (k, v) -> t k v -> t k v
	eliminar :: Ord k => k -> t k v -> t k v
	buscar   :: Ord k => k -> t k v -> Maybe v

{- Para entender mejor esta clase, intentaré la implementación más obvia de diccionarios y trataré de instanciarla a esta clase -}



data Dicc k v = D [(k,v)] deriving Show

instance Diccionario Dicc where

	--vacia :: Ord k => Dicc k v 
	vacia = D []


	--insertar :: Ord k => (k,v) -> Dicc k v -> Dicc k v
	insertar t@(key,value) (D xs)  |  length [t | t<-xs , fst(t)==key ]==0 = D (t:xs)
                                | otherwise                      = D xs

	--eliminar' :: Ord k => k -> Dicc k v -> Dicc k v
	eliminar key (D xs) = D  [t | t<-xs , not ((fst t) ==key) ]

	--buscar :: Ord k => k -> Dicc k v -> Maybe v
	buscar key (D xs) | length a==1 = Just (snd (a!!0))
    	               | otherwise   = Nothing
		where a=[t | t<-xs , fst(t)==key]  
                   


directorio=D [("jere",21),("ash",10), ("pikachu",1),("moria",60)]




{- Ahora usaré el siguiente tipo e intentaré instanciarlo a diccionario -}

data BTree32 k a = Nil | Node (BTree32 k a) Int (k,a) (BTree32 k a) deriving (Show, Eq)

vacia' :: BTree32 k a 
vacia' = Nil

buscar'   :: Ord k => k -> BTree32 k v -> Maybe v
buscar' k Nil = Nothing
buscar' k (Node l cant (key,val) r) | k==key      = Just val
                                    | k<key       = buscar' k l
                                    | otherwise   = buscar' k r


size :: BTree32 k v -> Int
size Nil = 0
size (Node _ s _ _) = s


singleL :: BTree32 k v -> BTree32 k v
singleL (Node saa n1 a (Node (Node sab n3 c sac) n2 b sad) ) = Node (Node  saa (n3+n1-n2) a (Node sab n3 c sac)) n1 b sad
singleL x = x

doubleL :: BTree32 k v -> BTree32 k v
doubleL (Node saa t1 a (Node (Node sab t3 c sac) t2 b sad)) = Node (Node saa (t1-t2+(size sab)) a sab) t1 c (Node sac (t2-t3+(size sac)) b sad)
doubleL x = x


singleR :: BTree32 k v -> BTree32 k v
singleR (Node (Node sad t2 b (Node sac t3 c sab)) t1 a saa) = Node sad t1  b (Node (Node sac t3 c sab) (1+(size saa)+1+t3) a saa)
singleR x = x


doubleR :: BTree32 k v -> BTree32 k v
doubleR (Node (Node sad t2 b (Node sac t3 c sab)) t1 a saa) = Node (Node sad ((size sad) + (size sac) + 1) b sac) t1 c (Node sab ((size sab) + (size saa) + 1) a saa)
doubleR x = x


balance :: BTree32 k v -> BTree32 k v -> (k,v) -> BTree32 k v

balance l r d 
              | size l + size r <= 1                                             = Node l 2 d r   --se cumple la prop de balanceo
              | ( (size l) <= 3* (size r) ) && ((size r) <= 3*(size l) )         = Node l t d r   --se cumple la prop de balanceo
-- Prop de balanceo (1) : size r <= 3 * size l
-- Prop de balanceo (2) : size l <= 3 * size r
              | (size r > 3 * size l)  && (sizei r < 2* sized r)                = singleL (Node l t d r)
              | (size r > 3 * size l)  && (sizei r >= 2* sized r)               = doubleL (Node l t d r)
              | (size l > 3 * size r)  && (sized l < 2* sizei l)                = singleR (Node l t d r)
              | (size l > 3 * size r)  && (sized l >= 2* sizei l)               = doubleR (Node l t d r)

	where sizei Nil = 0
	      sizei (Node l t d r) = size l
	      sized Nil = 0
              sized (Node l t d r) = size r
	      t = (size l + size r + 1)	                       


insert' :: Ord k => (k,v) -> BTree32 k v -> BTree32 k v
insert' par@(k,v) Nil = Node Nil 1 par Nil
insert' (k,v) arbol@(Node l nelems (k1,v1) r) | k==k1 = arbol
                                           | k<k1  = balance (insert' (k,v) l) r (k1,v1)
                                           | k>k1  = balance l (insert' (k,v) r) (k1,v1)



tester = Node  (Node Nil 1 ("ash",10) Nil) 5 ("jere",21) (Node (Node Nil 1 ("n",44) Nil)  3 ("moria",60) (Node Nil 1 ("pika",1) Nil)) 

sai (Node l t d r) = l
sad (Node l t d r) = r


tester2 = Node ( Node (Node (Node Nil 1 ("a",7) Nil) 2 ("b",4) Nil) 6 ("g",3) ( Node (Node Nil 1 ("h",8) Nil) 3 ("k",5) (Node Nil 1 ("n",9) Nil))  ) 9 ("p",0) (Node (Node Nil 1 ("q",10) Nil) 2 ("t",2) Nil)


delRoot ::                               Ord k => BTree32 k v -> BTree32 k v
delRoot Nil                                    = Nil
delRoot (Node Nil 1 _ Nil )                    = Nil
delRoot (Node l n d r)  | (size l) >  (size r) = Node (delRoot l) (n-1) (root l) r
                        | (size r) >= (size l) = Node l (n-1) (root r) (delRoot r)
    where root (Node _ _ d _ ) = d

delete :: Ord k => k -> BTree32 k a -> BTree32 k a 
delete _ Nil = Nil
delete key t@(Node l n (k1,v1) r ) | key == k1 = delRoot t
                                   | key < k1  = balance (delete key l) r (k1,v1) 
                                   | key > k1  = balance l (delete key r) (k1,v1) 



instance Diccionario BTree32 where

	--vacia :: Ord k => BTree32 k v 
	vacia = Nil


	--insertar :: Ord k => (k,v) -> BTree32 k v -> BTree32 k v
	insertar = insert'

	--eliminar' :: Ord k => k -> BTree32 k v -> BTree32 k v
	eliminar = delete

	--buscar :: Ord k => k -> BTree32 k v -> Maybe v
	buscar = buscar'






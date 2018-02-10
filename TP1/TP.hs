-- Trabajo Práctico I - EDAII 2015 - LCC 
-- ALUMNO: Rodríguez Jeremías


class Diccionario t where
        vacia    :: Ord k => t k v
	insertar :: Ord k => (k, v) -> t k v -> t k v
	eliminar :: Ord k => k -> t k v -> t k v
	buscar   :: Ord k => k -> t k v -> Maybe v

-- Utilizaremos esta clase de tipo para implementar diccionarios.



data BTree32 k a = Nil | Node (BTree32 k a) Int (k,a) (BTree32 k a) deriving (Show, Eq)
--                               sa. izq     T    D       sa. derecho



-- Donde T: tamaño del subárbol que tiene el nodo en cuestión como raíz
--       D: par (clave, valor) que se guarda en ese nodo.


{- Definimos el tipo BTree32, arbol binario ordenado que mantiene la propiedad de balanceo:
Para todo nodo:
        i) tamaño del sa. derecho <= 3 * tamaño sa. izquierdo
        ii) tamaño del sa. izquierdo <= 3 * tamaño sa. derecho
        iii) si uno de los subárboles es vacío, el otro puede tener tamaño 1.

Deseamos instanciarlo a la clase antes definida. Para ello primero se definen las funciones
vacia', insertar', eliminar' y buscar' junto con otras funciones auxiliares. 

-}

--size calcula el tamaño de un BTree32

size :: BTree32 k v -> Int
size Nil = 0
size (Node _ s _ _) = s


--La constante vacia' denota un árbol vacío. Se usa el constructor de datos Nil.
vacia' :: BTree32 k a 
vacia' = Nil 


{- Se busca recursivamente la clave en el árbol. Si se llega a una hoja, la clava no está
guardada en el diccionario y se devuelve Nothing. Si se llega a una coincidencia de claves,
se devuelve just valor, donde valor es el dato buscado-}

buscar'   :: Ord k => k -> BTree32 k v -> Maybe v
buscar' k Nil = Nothing
buscar' k (Node l cant (key,val) r) | k==key      = Just val
                                    | k<key       = buscar' k l
                                    | otherwise   = buscar' k r


{- Rotaciones indicadas en los esquemas del enunciado. Transforman el árbol de tal forma
que ciertos elementos de un subárbol pasen al otro para mantener la propiedad de balanceo -}

--Rotaciones hacia la izquierda 

singleL :: BTree32 k v -> BTree32 k v
singleL (Node saa n1 a (Node sac n2 b sad) ) = Node (Node saa t a sac) n1 b sad
        where t=size saa + size sac + 1
singleL x = x
        

doubleL :: BTree32 k v -> BTree32 k v
doubleL (Node saa t1 a (Node (Node sab t3 c sac) t2 b sad)) = Node (Node saa t1 a sab) sz1 c (Node sac sz2 b sad)
        where sz1=(t1-t2+(size sab))
              sz2=(t2-t3+(size sac)) 
doubleL x = x


--Rotaciones hacia la derecha

singleR :: BTree32 k v -> BTree32 k v
singleR (Node (Node sad n2 b sac) n1 a saa) = Node sad n1 b (Node sac t a saa)
        where t=size sac + size saa + 1
singleR x = x

doubleR :: BTree32 k v -> BTree32 k v
doubleR (Node (Node sad t2 b (Node sac t3 c sab)) t1 a saa) = Node (Node sad sz1 b sac) t1 c (Node sab sz2 a saa)
        where sz1=((size sad) + (size sac) + 1)
              sz2=((size sab) + (size saa) + 1)
doubleR x = x


{-balance recibe dos BTree32, un par (clave1, valor1) y devuelve un BTree32 (balanceado)
suponiendo que las claves del primer árbol son "menores" a clave1 (en el orden definido por 
la instancia Ord del tipo de las claves)  y las del segundo subarbol son mayores. -}   

balance :: BTree32 k v -> BTree32 k v -> (k,v) -> BTree32 k v

balance l r d 
              | size l + size r <= 1                                             = Node l 2 d r   --se cumple la prop de balanceo
              | ( (size l) <= 3* (size r) ) && ((size r) <= 3*(size l) )         = Node l t d r   --se cumple la prop de balanceo
-- Prop de balanceo (1) : size r <= 3 * size l
-- Prop de balanceo (2) : size l <= 3 * size r
-- De los siguientes casos, falla (1) en los dos primeros y (2) en los dos últimos.
-- Se aplica la rotación adecuada según la cantidad de elementos de los subárboles.

              | (size r > 3 * size l)  && (sizei r < 2* sized r)                = singleL (Node l t d r) 
              | (size r > 3 * size l)  && (sizei r >= 2* sized r)               = doubleL (Node l t d r)
              | (size l > 3 * size r)  && (sized l < 2* sizei l)                = singleR (Node l t d r)
              | (size l > 3 * size r)  && (sized l >= 2* sizei l)               = doubleR (Node l t d r)

	where sizei Nil = 0
	      sizei (Node l t d r) = size l
	      sized Nil = 0
              sized (Node l t d r) = size r
	      t = (size l + size r + 1)	                       

--De forma similar a lo que hicimos en RBT, se define la inserción.

insert' :: Ord k => (k,v) -> BTree32 k v -> BTree32 k v
insert' par Nil = Node Nil 1 par Nil
insert' p@(k,v) arbol@(Node l nelems (k1,v1) r) | k==k1 = arbol
                                                | k<k1  = balance (insert' p l) r (k1,v1)
                                                | k>k1  = balance l (insert' p r) (k1,v1)


--Elimina la raíz de un árbol

delRoot ::                               Ord k => BTree32 k v -> BTree32 k v

delRoot Nil                                    = Nil
delRoot (Node Nil 1 _ Nil )                    = Nil
delRoot (Node l n d r)  | (size l) >=  (size r)= Node (delRoot l) (n-1) (root l) r
                        | (size r) > (size l)  = Node l (n-1) (root r) (delRoot r)

--La nueva raíz es la raíz del subárbol que tiene más elementos, para preservar el balanceo.
--Se aplica recursivamente delRoot.
    where root (Node _ _ d _ ) = d


--Elimina un par (clave, valor) si la clave está alojada.

delete :: Ord k => k -> BTree32 k a -> BTree32 k a 
delete _ Nil = Nil
delete key t@(Node l n (k1,v1) r ) | key == k1 = delRoot t
                                   | key < k1  = balance (delete key l) r (k1,v1) 
                                   | key > k1  = balance l (delete key r) (k1,v1) 

--Si la clave no está, entonces, llego a una hoja y no cambio nada.
--Si la clave está, localizo el subárbol que la tiene como raíz y le quito la raíz. Luego 
--aplico balance, para verificar que la propiedad de balanceo se cumpla si se rompió.



sai (Node l t d r) = l
sad (Node l t d r) = r



--finalmente, utilizo estas funciones para instanciar BTree32 a la clase Diccionario.

instance Diccionario BTree32 where
	vacia = vacia'
	insertar = insert'
	eliminar = delete
	buscar = buscar'







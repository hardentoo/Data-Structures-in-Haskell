{- IMPLEMENTACIÓN DE SECUENCIAS UTILIZANDO LISTAS -}

import Par
import Seq

instance Seq [] where
        emptyS = []

        singletonS x = x:[]
	
        lengthS  []     = 0
        lengthS (x:xs) = 1 + lengthS xs
		
        nthS []     n = error "Indice invalido"
        nthS (x:xs) 0 = x
        nthS (x:xs) n = nthS xs (n-1)

        tabulateS f 0 = []
        tabulateS f n = tabulate_bw 0
	        where tabulate_bw m | m==n = []
                                    | m<n  = let (x,xs)= (f m) ||| (tabulate_bw (m+1) ) in x:xs

        mapS f []      = []
        mapS f (x:xs)  =  let (x',xs')= (f x) ||| (mapS f xs) in (x':xs')

        filterS p []     = []
        filterS p (x:xs) = if cond then (x:xs') else xs'
	        where (cond,xs') = (p x) ||| (filterS p xs)
	
        appendS [] ys      = ys
        appendS xs []      = xs
        appendS (x:xs) ys  =  x:(appendS xs ys)

        takeS _  0   = []
        takeS [] _   = []
        takeS (x:xs) n = x : (takeS xs (n-1))
		
        dropS xs     0 = xs
        dropS []     n = []
        dropS (x:xs) n = dropS xs (n-1)


        showtS []     = EMPTY
        showtS (x:[]) = ELT x
        showtS xs     = let (l,r)= take n xs ||| drop n xs in (NODE l r)
        	where n=div (length xs) 2

	
        showlS []     = NIL
        showlS (x:xs) = CONS x xs
		
        joinS []       = []
        joinS (xs:xss) = appendS xs (joinS xss)

        reduceS op e []                 = e
        reduceS op e (x:[])             = op e x
        reduceS op e xs                 = reduceS op e (contraer op xs)
	
	--Explicación de cómo funciona scanS: PDF de complejidades.
        --Explicación de cómo funciona expandir: más abajo.			
	scanS op e [] =  ([],e)
        scanS op e [x]=  ([e],op e x) 			
        scanS op e s  =  (xs, snd s')
        	where (s',n)             = scanS op e (contraer op s) ||| lengthS s
	              xs                 = expandir op (fst s') s 0 n 

              
         
        fromList xs = xs





--FUNCIONES AUXILIARES DE scanS y reduceS:

contraer op []        = []
contraer op (x:[])    =  x:[]
contraer op (x:y:xs)  = let (x',xs') = (op x y) ||| (contraer op xs) in (x':xs')

        
--expandir : Operación -> Scan(Lista contraída) -> Lista original -> Índice a crear -> Primer índice inválido -> Lista expandida


{- Expandir trabaja de la siguiente forma: En primer lugar, es llamada desde scanS con i=0 
es decir, comenzamos a crear la nueva secuencia expandida desde el índice 0.

En cada llamada a expandir se creará el índice i-ésimo de la secuencia expandida, hasta llegar
al índice n-ésimo que no existirá, y se terminará la expansión.

En cada llamada consideramos si estamos creando un índice par o impar.
Si el índice el par, entonces será la secuencia contraída a la que le aplicamos scan, indexada
en (div i 2) el elemento buscado.

Si el índice es impar, entonces fabricamos el elemento buscado operando el elemento (i div 2)-ésimo
de la secuencia contraída scaneada con el elemento i-1 ésimo de la secuenia original.

De este modo generamos toda la secuencia expandida.

-}


expandir _ _ _ i n            | i==n   = []
expandir op (c:cs) ss 0 n              = c : (expandir op (c:cs) ss 1 n)
expandir op (c:cs) ss i n     | even i = c : (expandir op (c:cs) (drop 1 ss) (i+1) n )
expandir op (c:cs) (s:ss) i n | odd i  = (op c s) : (expandir op cs ss (i+1) n)
expandir op [] _ _ _                   = error "no debería pasar"


{-
 Pasamos tantos argumentos porque es más eficiente recorrer una vez las listas ss y cs
que indexarlas muchas veces para buscar los elementos deseados. Si indexar fuera constante,
esta función sería mucho mas sencilla, como sucede en la implementación con arreglos.

En el pdf donde analizamos las complejidades hay un ejemplo. Intentamos respetar exáctamente
el método presentado en teoría para que la asociatividad sea la correcta.
-}
		

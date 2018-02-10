{- IMPLEMENTACIÓN DE SECUENCIAS UTILIZANDO ARREGLOS -}
--Agregamos en el modulo Arr.hs la posibilidad de exportar la función empty

import Par
import qualified Arr as A
import Arr (tabulate, (!))
import Seq

instance Seq A.Arr where
    emptyS         = A.empty
    singletonS x   = A.tabulate (\i -> x) 1
    lengthS        = A.length
    nthS           = (!)
    takeS s k      = tabulate (\i -> nthS s i) (min k (lengthS s) )
    dropS s k      = A.subArray from (n-from) s
                         where  n    = lengthS s
                                from = min k n

    showtS s       | n==0 = EMPTY 
                   | n==1 = ELT (s!0)
                   | n> 1 = NODE l r
                        where n = lengthS s
                              l = takeS s (div n 2)
                              r = dropS s (div n 2)

    showlS s       | n == 0 = NIL
                   | n >  0 = CONS (s!0) (dropS s 1)
                        where n = lengthS s

    appendS s t    = tabulate g (n1+n2)
                         where n1  = lengthS s
                               n2  = lengthS t
                               g i = if i < n1 then s!i else t!(i-n1)
    fromList       = A.fromList 
    joinS          = A.flatten
    tabulateS      = A.tabulate
    mapS f s       = tabulateS (\i -> f (s!i) ) (lengthS s)
    filterS f s    = joinS (tabulateS g n)
                        where 
                            n   = lengthS s 
                            g i = if f elem then (singletonS elem) else emptyS
                             where elem = (s!i)
    reduceS op e s | n == 0 = e
                   | n == 1 = op e (s!0)
                   | n >  1 = reduceS op e t
                      where 
                          n = lengthS s
                          t = contraer op s

    scanS op e s   | n == 0 = (emptyS,e)
                   | n == 1 = (singletonS e, op e (s!0) )
                   | n  > 1 = xs ||| snd t
                        where n  = lengthS s
                              t  = scanS op e (contraer op s)
                              xs = expander op s (fst t)


contraer op s = tabulate g m
                where n   = lengthS s
                      m   = div (n + 1) 2
                      g i | i==(m-1)  = if (odd n) then (s!(n-1)) else elem
                          | otherwise = elem
                                where elem = op (s!(2*i)) (s!(2*i+1))
expander op s s' = tabulate f n
                where   n    = lengthS s
                        f i | even i    = elem
                            | otherwise = op elem (s!(i-1))
                              where elem = s'!(div i 2)
--op i j = "("++i++"+"++j++")"

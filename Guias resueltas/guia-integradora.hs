--Ejercicio 1
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

productos::[String]->[(String,Int)]
productos [] = []
productos (x:xs) = (x, apariciones x (x:xs)) : productos (quitarTodos x xs)

quitarTodos::(Eq t)=>t->[t]->[t]
quitarTodos a [] = []
quitarTodos a (x:xs) | a== x = quitarTodos a xs
                     | otherwise= x: quitarTodos a xs

apariciones::(Eq t)=>t->[t]->Int
apariciones _ [] = 0
apariciones a (x:xs)    | a==x = 1 + apariciones a xs
                        | a/=x = apariciones a xs

--Ejercicio 2

stockDeProducto::String->[(String,Int)]->Int
stockDeProducto [] a = 0
stockDeProducto a ((x,y):xs)    | x==a = y
                                | x/=a = stockDeProducto a xs

--Ejercicio 3
dineroEnStock::[(String,Int)]->[(String,Float)]->Float
dineroEnStock [] _ = 0
dineroEnStock ((x1,x2):xs) ys = precio x1 ys* fromIntegral (stockDeProducto x1 ((x1,x2):xs)) + dineroEnStock xs ys

precio::String->[(String,Float)]->Float
precio a ((x1,x2):xs)   | a==x1 = x2
                        |otherwise = precio a xs

--Ejercicio 4
aplicarOferta::[(String,Int)]->[(String,Float)]->[(String,Float)]
aplicarOferta [] _ = []
aplicarOferta ((x1,x2):xs) ys   | stockDeProducto x1 ((x1,x2):xs)>10 = (x1,precio x1 ys*0.8): aplicarOferta xs ys
                                | otherwise = (x1,precio x1 ys): aplicarOferta xs ys

--Ejercicio 5
maximo::[[Int]]->Int
maximo xs = maximoListas (unaLista xs)

unaLista::[[t]]->[t]
unaLista [] = []
unaLista (x:xs) = x++ unaLista xs

maximoListas::[Int]->Int
maximoListas [x] = x
maximoListas (x:xs) | x>=maximoListas xs = x
                    | otherwise = maximoListas xs
--Ejercicio 6

masRepetido::[[Int]]->Int
masRepetido xs = masRepetidoAux (elementoYCantidad (unaLista xs))

masRepetidoAux::[(t,Int)]->t
masRepetidoAux [x] = fst x
masRepetidoAux (x:y:xs) | snd x >= snd y = masRepetidoAux (x:xs)
                        | otherwise = masRepetidoAux (y:xs)

elementoYCantidad::(Eq t)=>[t]->[(t,Int)]
elementoYCantidad [] = []
elementoYCantidad (x:xs) = (x,apariciones x (x:xs)): elementoYCantidad (quitarTodos x xs)

--Ejercicio 7
valoresDeCamino::[[Int]]->[(Int,Int)]->[Int]
valoresDeCamino _ [] = []
valoresDeCamino xs ((x,y):ys) = hallarValor xs [(x,y)] ++ valoresDeCamino xs ys

hallarValor::[[Int]]->[(Int,Int)]->[Int]
hallarValor xs [(x,y)] = [elementoIesimo y (elementoIesimo x xs)]

elementoIesimo::Int->[t]->t
elementoIesimo 1 xs = head xs
elementoIesimo n xs = elementoIesimo (n - 1) (tail xs)

--Ejercicio 8
fib::Int->Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

esCaminoFib::[Int]->Int->Bool
esCaminoFib [] _ = True
esCaminoFib (x:xs) i    | x == fib i = esCaminoFib xs (i + 1)
                        | otherwise   = False

--Ejercicio 9
divisoresPropios::Int->[Int]
divisoresPropios 1 = [1]
divisoresPropios x = reverso (buscaDivisores x (x-1))

buscaDivisores::Int->Int->[Int]
buscaDivisores x 1 = [1]
buscaDivisores x i  | mod x i == 0 = i : buscaDivisores x (i-1)
                    | otherwise= buscaDivisores x (i-1)

reverso::[t]->[t]
reverso [] = []  
reverso (x:xs) = reverso xs ++ [x]

--Ejercicio 10
sonAmigos::Int->Int->Bool
sonAmigos a b = sumatoria (divisoresPropios a) == b && sumatoria (divisoresPropios b) == a

sumatoria::[Int]->Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--Ejercicio 11
losPrimerosNPerfectos::Int->[Int]
losPrimerosNPerfectos n = reverso (losPrimerosNPerfectosAux n)

losPrimerosNPerfectosAux::Int->[Int]
losPrimerosNPerfectosAux 1 = [1]
losPrimerosNPerfectosAux n = proximoPerfecto (nEsimoPerfecto (n-1)):losPrimerosNPerfectosAux (n-1)

nEsimoPerfecto :: Int->Int
nEsimoPerfecto 1 = 1
nEsimoPerfecto n = proximoPerfecto (nEsimoPerfecto (n - 1))

proximoPerfecto::Int->Int
proximoPerfecto n   | esPerfecto (n+1) = n+1
                    | otherwise = proximoPerfecto (n+1)

esPerfecto::Int->Bool
esPerfecto n =sumatoria (divisoresPropios n) == n

--Ejercicio 12
listaDeAmigos::[Int]->[(Int,Int)]
listaDeAmigos [x] = []
listaDeAmigos (x:xs) = buscarAmigos x xs ++ listaDeAmigos xs

buscarAmigos::Int->[Int]->[(Int,Int)]
buscarAmigos a [] = []
buscarAmigos a (x:xs)   | a==x = buscarAmigos a xs
                        | sonAmigos a x = (a,x): buscarAmigos a xs
                        | otherwise = buscarAmigos a xs
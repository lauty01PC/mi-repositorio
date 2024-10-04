{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use :" #-}

maximo::[[Int]]->Int
maximo xs  = maximoListas (aplanar xs)

maximoListas::[Int]->Int
maximoListas [x] = x
maximoListas (x:xs) | x>= maximoListas xs = x
                    | otherwise = maximoListas xs
aplanar::[[t]]->[t]
aplanar [] = [] 
aplanar (x:xs) = x ++ aplanar xs

--2

apariciones::(Eq t)=>t->[t]->Int
apariciones a [] = 0
apariciones a (x:xs)    | a==x = 1 + apariciones a xs
                        | otherwise = apariciones a xs

quitarTodosLos::(Eq t)=>t->[t]->[t]
quitarTodosLos _ [] = []
quitarTodosLos a (x:xs) | a==x = quitarTodosLos a xs
                        | otherwise= x : quitarTodosLos a xs

elementoYApariciones::(Eq t)=>[t]->[(t,Int)]
elementoYApariciones [] = []
elementoYApariciones (x:xs) = (x,apariciones x (x:xs)):elementoYApariciones (quitarTodosLos x xs)

masRepetidoAux::[(Int,Int)]->Int
masRepetidoAux [x] = fst x
masRepetidoAux (x:y:xs) | snd x >= snd y = masRepetidoAux (x:xs)
                        | otherwise = masRepetidoAux (y:xs)

masRepetido::[[Int]]->Int
masRepetido xs = masRepetidoAux (elementoYApariciones (aplanar xs))

--3
valoresDeCamino::[[Int]]->[(Int,Int)]->[Int]
valoresDeCamino _ [] = []
valoresDeCamino xs (y:ys) = hallarValor y xs : valoresDeCamino xs ys

hallarValor::(Int,Int)->[[Int]]->Int
hallarValor (x,y) xs = nElemento y (nElemento x xs)

nElemento::Int->[t]->t
nElemento 1 xs = head xs
nElemento n xs = nElemento (n-1) (tail xs)

-- 4)
fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

listaFibonacci :: Int -> [Int]
listaFibonacci 0 = [1]
listaFibonacci 1 = [1]
listaFibonacci n = [fibonacci n] ++ listaFibonacci (n - 1)

esCaminoFibo :: [Int] -> Int -> Bool
esCaminoFibo (x:xs) n = (x:xs) == listaFibonacci n
module SolucionT1 where


-- Ejercicio 1
mediaMovilN :: [Integer] -> Integer -> Float
mediaMovilN (x:xs) n = dividir (sumatoria (primerosNElementos n (reverso (x:xs)))) n

sumatoria::[Integer]->Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

dividir::Integer->Integer->Float
dividir a b = fromIntegral a / fromIntegral b

reverso::[Integer]->[Integer]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

primerosNElementos::Integer->[Integer]->[Integer]
primerosNElementos 1 (x:xs) = [x]
primerosNElementos n (x:xs) = [x]++ primerosNElementos (n-1) xs


 --Ejercicio 2
esAtractivo :: Integer -> Bool
esAtractivo n = esPrimo (cantElementos (factoresPrimos n)) 

menorDivisorAux::Integer->Integer->Integer
menorDivisorAux n i | mod n i == 0 = i 
                    | otherwise= menorDivisorAux n (i+1)

menorDivisor::Integer->Integer
menorDivisor n = menorDivisorAux n 2

esPrimo::Integer->Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n 

descomponerAux::Integer->Integer->[Integer]
descomponerAux 1 _ = []
descomponerAux n i  | mod n i == 0 = i:descomponerAux (div n i) i
                    | otherwise = descomponerAux n (i+1)

factoresPrimos::Integer->[Integer]
factoresPrimos n = descomponerAux n 2

cantElementos::[Integer]->Integer
cantElementos [] = 0
cantElementos (x:xs) = 1 + cantElementos xs


-- Ejercicio 3
palabraOrdenada :: String -> Bool
palabraOrdenada z = palabraOrdenadaAux (sacarBlancos z)

palabraOrdenadaAux::String->Bool
palabraOrdenadaAux [] = False
palabraOrdenadaAux [z] = False
palabraOrdenadaAux [a,b] = a<=b
palabraOrdenadaAux z    | head (sacarBlancos z) <= head (tail (sacarBlancos z)) = palabraOrdenada (tail (sacarBlancos z))
                        | otherwise = False

sacarBlancos::String->String
sacarBlancos [] = []
sacarBlancos z  | head z == ' ' = sacarBlancos (tail z)
                | otherwise = (head z) : sacarBlancos (tail z)


-- Ejercicio 4
similAnagrama :: String -> String -> Bool
similAnagrama xs [] = False
similAnagrama [] xs = False
similAnagrama x y   | x==y = False
                    |otherwise = quitarRepetidos (elementoYCantidad (sacarBlancos x) ++ elementoYCantidad (sacarBlancos y)) == elementoYCantidad (sacarBlancos x)


quitarRepetidos::(Eq t)=>[t]->[t]
quitarRepetidos [] = []
quitarRepetidos (x:xs) = x : quitarRepetidos (quitarTodos x xs)


perteneceTuplas::(Char,Integer)->[(Char,Integer)]->Bool
perteneceTuplas x [] = False
perteneceTuplas x (y:ys)    | x==y = True
                            | otherwise= perteneceTuplas x ys

elementoYCantidad::String->[(Char,Integer)]
elementoYCantidad [] = []
elementoYCantidad (x:xs) = (x,cantApariciones x (x:xs)):elementoYCantidad (quitarTodos x (x:xs))

cantApariciones::(Eq t)=>t->[t]->Integer
cantApariciones a [] = 0
cantApariciones a (x:xs)    | a== x = 1 + cantApariciones a xs
                            | otherwise= cantApariciones a xs

quitarTodos::(Eq t)=>t->[t]->[t]
quitarTodos a [] = []
quitarTodos a (x:xs)    | a==x = quitarTodos a xs
                        | otherwise= x: quitarTodos a xs
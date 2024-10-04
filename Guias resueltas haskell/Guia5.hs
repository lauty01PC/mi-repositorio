--Ejercicio 1

--i
longitud::[t]->Int
longitud [] = 0
longitud (x:xs)= 1 + longitud (tail (x:xs))

--ii

ultimo::[t]->[t]
ultimo [x] = [x]
ultimo (x:xs) = ultimo xs

--iii

principio::[t]->[t]
principio [x] = [x]
principio [x, y]= [x]
principio (x:xs)= x : principio xs

--iv
reverso::[t]->[t]
reverso []=[]
reverso [x] = [x]
reverso (x:xs) = ultimo (x:xs) ++ reverso (principio (x:xs))

--Ejercicio 2
--i
pertenece:: (Eq t)=> t->[t]->Bool
pertenece _ [] = False
pertenece a (x:xs) = a == x || pertenece a xs

--ii
todosIguales::(Eq t)=>[t]->Bool
todosIguales [x,y]= x==y
todosIguales (x:y:xs)= x==y && todosIguales (y:xs)

--iii
todosDistintos::(Eq t)=>[t]->Bool
todosDistintos [x] = True
todosDistintos [x, y]= x/=y
todosDistintos (x:y:xs) = x/=y && todosDistintos (y:xs)

--iv
hayRepetidos::(Eq t) =>[t]->Bool
hayRepetidos [] = False
hayRepetidos [x] = False
hayRepetidos [x,y] = x==y
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos xs

--v
quitar::(Eq t)=>t->[t]->[t]
quitar a (x:xs) | not (pertenece a (x:xs)) = x:xs
                | a/=x = x: quitar a xs
                | a==x = xs

--vi

quitarTodos::(Eq t)=>t->[t]->[t]
quitarTodos a (x:xs)    | pertenece a (x:xs) = quitarTodos a (quitar a (x:xs))
                        | otherwise = x:xs

--vii
eliminarRepetidos::(Eq t)=>[t]->[t]
eliminarRepetidos (x:xs)    | hayRepetidos (x:xs) && pertenece x xs = eliminarRepetidos (quitar x (x:xs))
                            | hayRepetidos (x:xs) && not (pertenece x xs)= x : eliminarRepetidos xs
                            | otherwise= x:xs

--viii
mismosElementos::(Eq t)=>[t]->[t]->Bool
mismosElementos (x:xs) (y:ys) = auxiliar (x:xs) (y:ys)==eliminarRepetidos (y:ys)

auxiliar :: Eq t => [t] -> [t] -> [t]
auxiliar (x:xs) (y:ys) = eliminarRepetidos ( eliminarRepetidos (x:xs)++ eliminarRepetidos (y:ys))

--ix
capicua::(Eq t)=>[t]->Bool
capicua xs = reverso xs == xs

--Ejercicio 3

--i
sumatoria::[Int]->Int
sumatoria [] = 0
sumatoria (x:xs) = x+sumatoria xs

--ii
productoria::[Int]->Int
productoria [] = 1
productoria (x:xs) = x*productoria xs

--iii
maximo::[Int]->Int
maximo [x] = x
maximo (x:xs)   | x>= maximo xs = x
                | otherwise = maximo xs

--iv
sumarN::Int->[Int]->[Int]
sumarN _ [] = []
sumarN n (x:xs) = (x+n) : sumarN n xs

--v
sumarElPrimero::[Int]->[Int]
sumarElPrimero (x:xs) = sumarN x (x:xs)

--vi
sumarElUltimo::[Int]->[Int]
sumarElUltimo xs = reverso (sumarElPrimero (reverso xs))

--vii
pares::[Int]->[Int]
pares [] = []
pares (x:xs)    | mod x 2 == 0 = x:pares xs
                | otherwise= pares xs

--viii
multiplosDeN::Int->[Int]->[Int]
multiplosDeN _ [] = []
multiplosDeN n (x:xs)   | mod x n == 0 = x:multiplosDeN n xs
                        | otherwise= multiplosDeN n xs

--ix
ordenarCreciente::[Int]->[Int]
ordenarCreciente [] = []
ordenarCreciente xs = ordenarCreciente quitarMaximo ++ [maximo xs]
                        where quitarMaximo = quitar (maximo xs) xs

--Ejercicio 4
--a
sacarBlancosRepetidos::[Char]->[Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:xs)  | (x==y) && (x==' ') = sacarBlancosRepetidos (y:xs)
                                | otherwise = x:sacarBlancosRepetidos (y:xs)

--b
contadorEspacios::[Char]->Int
contadorEspacios [] = 0
contadorEspacios (x:xs) | x==' ' = 1 + contadorEspacios xs
                        | otherwise = contadorEspacios xs

contarPalabras::[Char]->Int
contarPalabras [] = 0
contarPalabras (x:xs)   | x==' ' && ultimo (x:xs)==[' ']= contadorEspacios (sacarBlancosRepetidos (x:xs)) - 1
                        | x==' ' || ultimo (x:xs)==[' ']= contadorEspacios (sacarBlancosRepetidos (x:xs))
                        | otherwise= contadorEspacios (sacarBlancosRepetidos (x:xs)) + 1

--c
palabras::[Char]->[[Char]]
palabras [] = []
palabras xs = limpiarOracion (primeraPalabra xs) : palabras (limpiarOracion (sacarPrimeraPalabra xs))

sacarUltimoEspacio:: [Char]->[Char]
sacarUltimoEspacio [] = []
sacarUltimoEspacio xs = reverso (sacarPrimerEspacio (reverso xs))


sacarPrimerEspacio :: [Char] -> [Char]
sacarPrimerEspacio [] = []
sacarPrimerEspacio (x:xs)   | x==' ' = xs
                            |otherwise = x:xs

limpiarOracion::[Char]->[Char]
limpiarOracion xs = sacarPrimerEspacio (sacarUltimoEspacio (sacarBlancosRepetidos xs))

primeraPalabra :: [Char] -> [Char]
primeraPalabra []= []
primeraPalabra (x:xs)   | x==' '= []
                        | otherwise = x:primeraPalabra xs

sacarPrimeraPalabra::[Char]->[Char]
sacarPrimeraPalabra [] = []
sacarPrimeraPalabra (x:xs)  | x==' ' = xs
                            | otherwise = sacarPrimeraPalabra xs
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use /=" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant ==" #-}
module SolucionT1 where
    
--Ejercicio 1

relacionesValidas :: [(String, String)] -> Bool

relacionesValidas [] = True
relacionesValidas [(x,y)] = not (x==y)
relacionesValidas ((x,y):xs)    | (pertenece (x,y) xs) || (pertenece (invertido (x,y)) xs) = False
                                | otherwise = relacionesValidas xs
pertenece::(Eq t)=>t->[t]->Bool
pertenece a [] = False
pertenece a (x:xs)  | a==x = True
                    | otherwise = pertenece a xs

invertido::(t,s)->(s,t)
invertido (x,y) = (y,x)

--Ejercicio 2

personas :: [(String, String)] -> [String]
personas [] = []
personas ((x,y):xs) = quitarRepetidos (x:y:personas xs)

quitarRepetidos::(Eq t)=>[t]->[t]
quitarRepetidos [] = []
quitarRepetidos (x:xs)  | pertenece x xs = x:(quitarTodos x xs)
                        | otherwise = x:quitarRepetidos xs

quitarTodos::(Eq t)=>t->[t]->[t]
quitarTodos a [] = []
quitarTodos a (x:xs) | a==x = quitarTodos a xs
                     | otherwise = x : quitarTodos a xs

--Ejercicio 3

amigosDe :: String -> [(String, String)] -> [String]
amigosDe a [] = []
amigosDe a ((x,y):xs)   | perteneceListaTuplas a ((x,y):xs)==False = []
                        | a==x = y: amigosDe a xs
                        | a==y = x: amigosDe a xs
                        | otherwise= amigosDe a xs
perteneceListaTuplas::(Eq t)=>t->[(t,t)]->Bool
perteneceListaTuplas a [] = False
perteneceListaTuplas a ((x,y):xs)   | a==x = True
                                    | a==y = True
                                    |otherwise = perteneceListaTuplas a xs  

--Ejercicio 4 

personaConMasAmigos::[(String,String)]->String
personaConMasAmigos x = personaConMasAmigosAux (personaYCantidadDeAmigos x)

personaConMasAmigosAux::[(String,Int)]->String
personaConMasAmigosAux [x] = fst x
personaConMasAmigosAux (x:y:xs)     | snd x >= snd y = personaConMasAmigosAux (x:xs)
                                    | otherwise = personaConMasAmigosAux (y:xs)

personaYCantidadDeAmigos::[(String,String)]->[(String,Int)]
personaYCantidadDeAmigos ((x,y):xs) = personaYCantidadDeAmigosAux (listaDeStrings ((x,y):xs))

listaDeStrings::[(String,String)]->[String]
listaDeStrings [] = []
listaDeStrings ((x,y):xs) = x:y: listaDeStrings xs

personaYCantidadDeAmigosAux::[String]->[(String,Int)]
personaYCantidadDeAmigosAux [] = []
personaYCantidadDeAmigosAux (x:xs) = (x,apariciones x (x:xs)) : personaYCantidadDeAmigosAux (quitarTodos x xs)

apariciones::(Eq t)=>t->[t]->Int
apariciones _ [] = 0
apariciones a (x:xs)    | a==x = 1 + apariciones a xs
                        | a/=x = apariciones a xs
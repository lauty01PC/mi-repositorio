{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
import Control.Concurrent (yield)
--Ejercicio 1
aproboMasDeNMaterias::[(String,[Int])]->String->Int->Bool
aproboMasDeNMaterias xs a n | cantDeMateriasAprobadas a xs  >= n = True
                            | cantDeMateriasAprobadas a xs < n = False

cantDeMateriasAprobadas::String->[(String,[Int])]->Int
cantDeMateriasAprobadas a [] = 0
cantDeMateriasAprobadas a ((x,y):xs) | a==x = mayoresA 4 y
                                     | otherwise =cantDeMateriasAprobadas a xs

mayoresA::Int->[Int]->Int
mayoresA n [] = 0
mayoresA n (x:xs)   | x>=n = 1 +mayoresA n xs
                    | x<n = mayoresA n xs

--Ejercicio 2
buenosAlumnos::[(String,[Int])]->[String]
buenosAlumnos [] = []
buenosAlumnos ((x,y):xs) | promedio y >= 8 && aplazos y < 1 = x : buenosAlumnos xs
                         | otherwise = buenosAlumnos xs

promedioDe::String->[(String,[Int])]->Int
promedioDe a [] = 0
promedioDe a ((x,y):xs) | a==x = promedio y
                        | otherwise = promedioDe a xs

promedio::[Int]->Int
promedio xs = div (sumatoria xs) (cantElementos xs)

sumatoria::[Int]->Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

cantElementos::[t]->Int
cantElementos [] = 0
cantElementos (x:xs) = 1 + cantElementos xs

aplazos::[Int]->Int
aplazos [] = 0
aplazos (x:xs)  | x<4 = 1 + aplazos xs
                | otherwise = aplazos xs

--Ejercicio 3

mejorPromedio::[(String,[Int])]->String
mejorPromedio [(a,b)] = a
mejorPromedio ((a,b):(c,d):xs)  | promedioDe a ((a,b):(c,d):xs) >= promedioDe c ((a,b):(c,d):xs) = mejorPromedio ((a,b):xs)
                                | otherwise = mejorPromedio ((c,d):xs)

--Ejercicio 4

seGraduoConHonores::[(String,[Int])]->Int->String->Bool
seGraduoConHonores ((x,y):xs) n a   | aproboMasDeNMaterias ((x,y):xs) a (cantElementos y - 1) && pertenece a (buenosAlumnos ((x,y):xs)) && (promedioDe (mejorPromedio ((x,y):xs)) ((x,y):xs) - promedioDe a ((x,y):xs) < 1) = True
                                    | otherwise = False

pertenece::(Eq t)=>t->[t]->Bool
pertenece a [] = False
pertenece a (x:xs) | a==x = True
                    | otherwise = pertenece a xs
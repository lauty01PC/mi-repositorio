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

quitarTodos::(Eq t)=>t->[t]->[t]
quitarTodos a [] = []
quitarTodos a (x:xs) | a== x = quitarTodos a xs
                     | otherwise= x: quitarTodos a xs

apariciones::(Eq t)=>t->[t]->Int
apariciones _ [] = 0
apariciones a (x:xs)    | a==x = 1 + apariciones a xs
                        | a/=x = apariciones a xs

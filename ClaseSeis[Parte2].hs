-- Clase #6 Parte 2 24/04 --

-- Recursividad --

-- Ejemplos de recursividad: 

factorial 0 = 1
factorial n = n * factorial (n - 1)

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Hay estructuras que son recursivas por naturaleza: Listas

lista    = [1,2,3]
lista'   = 1:[2,3]
lista''  = 1:2:[3]
lista''' = 1:2:3:[] -- Siendo [] El caso base

-- Recreamos la función concat:

concatenameAlgo = concat [[1,2], [3,4], [5,6,7,8]]

concatenacion [] = []
concatenacion (x:xs) = x ++ concatenacion xs 


-- Recreamos la función sumatoria:

sumatoria [] = 0
sumatoria (x:xs) = (+) x (sumatoria xs)

-- Toda recursividad se compone de un caso base y un caso recursivo --

reLoco operacion valorInicial [] = valorInicial
reLoco operacion valorInicial (x:xs) = operacion x (reLoco operacion valorInicial xs)

-- Y con esto podemos armar varias funciones conocidas:

concatenacion' xs = reLoco (++) [] xs
disyuncion xs     = reLoco (||) False xs
conjuncion xs     = reLoco (&&) True xs
productoria xs    = reLoco (*) 1 xs
sumatoria' xs     = reLoco (+) 0 xs

-- Esta función reLoco ya existe y se llama fold

-- foldl recibe función, valor inicial y lista --> Recorre de izquierda a derecha comenzando por el valor inicial operado con el primer valor de la lista
-- foldr recibe función, valor inicial y lista --> Recorre de derecha a izquierda comenzando por el valor inicial operado con el último valor de la lista
-- fold1 recibe función y lista --> Recorre de izquierda a derecha comenzando a operar el primer valor con el segundo
-- foldr1 recibe función ylista --> Recorre de derecha a izquierda comenzando a operar el último valor con el anteúltimo


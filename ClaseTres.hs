-- Clase #3 03/04/2019 --

{-
-Expresividad: De la mano con Intention revealing, que se vea la intención en el código:
 "Que si le muestro el código a mi abuela sepa qué hace"

 -Declaratividad: La menor cantidad de detalles algoritmicos posibles

 -Transparencia referencial --> De la mano con la inmutabilidad.
    El signo '=' significa igualdad o equivalencia, pero no 'asignación' como sucedía en imperativo.

    De la misma manera, si yo tengo un valor:

    miNombre = "Roberto"

    en cualquier parte del código del programa puedo usar indistintamente 'miNombre' y "Roberto", el programa
    no dejará de funcionar

 -Efecto de lado --> (side effect) --> "El mundo no cambia"
-}

import Text.Show.Functions -- Permite utilizar la aplicación parcial sin dar mensaje de error --

suma :: Num a => a -> a -> a
suma = (+) -- Al ingresar 'suma 2' devuelve <function> ya que es una función que sumará dos al próximo parámetro --

-- :t suma 2   --> devuelve 'suma 2 :: Num a => a -> a' --
-- :t suma 2 3 --> devuelve 'suma 2 3 :: Num a => a --

triple = (3 *)

-- t: 6 --> devuelve 6 :: Num a -> a

{-
Se puede usar la aplicación parcial para diferentes cosas:

> (suma 4) 2 
> 6

> (suma 2) (suma 2 4)
> 8

> (suma 2) $ (suma 2) 4
> 8

> (+) ((+) 3 4 ) 5
> 12


La aplicación parcial es una facilidad de nos da haskell que ya tienen por naturaleza
las funciones en Haskell.
Las funciones, reciben SIEMPRE un solo parámetro, quedando a la espera del siguiente
A este concepto se le llama CURRIFICACIÓN y es natural en Haskell
-}

suma' n = (n +) -- Lo que se encuentra a la derecha del '=' es aplicación parcial

-- A LA IZQUIERDA DEL IGUAL NUNCA HAY APLICACIÓN PARCIAL!!--

-- Ejercicios:

quintupleEsPar = even . (* 5)
cuadrupleDelSiguiente n = ((* 4) . (+ 1)) n
siempre3 = const 3
esPalabraVacia = (== "")
esMultiploDeDos = even
esMultiploDeTres num = 0 ==  mod num 3

-- firma de la composición: (.):: (b -> c) -> (a -> b) -> (a -> c)

-- Si la longitud de la primera palabra es mayor que la segunda --
primeraMayorQueSegunda :: String -> String -> Bool 
primeraMayorQueSegunda palabra1 palabra2 = length palabra1 > length palabra2

-- Si el primer número es mayor que el segundo --
mayorQueElSegundo num1 num2 = id num1 > id num2

-- Si el módulo del primer número es mayor que el módulo del segundo --
modMayorQueElSegundo num1 num2 = abs num1 > abs num2

-- Si la primera letra de la primera palabra viene después alfabeticamente de la primera letra de la segunda palabra --
primeraLetraVieneAntes palabra1 palabra2 = head(palabra1) > head(palabra2)

-- La lógica de las funciónes es la misma => buscamos generalizar:

esMayorSegun :: (Ord b) => (a -> b) -> a -> a -> Bool
esMayorSegun funcion valor1 valor2 = funcion valor1 > funcion valor2

--A esto se le llama función de orden superior -> pasamos funciónes como parámetros: '$' '.'

primeraMayorQueSegunda' p1 p2 = esMayorSegun length p1 p2 -- NO REPETIR CÓDIGO, solo la parte derecha
mayorQueElSegundo' num1 num2 = esMayorSegun id num1 num2
modMayorQueElSegundo' num1 num2 = esMayorSegun abs num1 num2
primeraLetraVieneAntes' p1 p2 = esMayorSegun head p1 p2


esMultiploDeTres' num = (== 0) (mod num 3)
esMultiploDeTres'' num = ((== 0) . (flip mod 3)) num






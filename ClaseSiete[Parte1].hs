-- LAZY EVALUATION --

{-
Las funciónes en haskell evalúan los parámetos en el momento en que lo necesítan. Por ese motivo, si bien no podemos mostrar en pantalla una lista infinita,
sí podemos conocer ciertas cosas sobre ellas:
-}

listaInfinita = [1..]

cabezaDeLista = head listaInfinita
elNúmeroMil   = listaInfinita !! 1000
losPrimeros n = take n listaInfinita
hastaQueSeaMayorACien = takeWhile  (< 100) listaInfinita

{-La 'evaluación perezosa' se compone de: "Call by name" + "Sharing
Por ejemplo podemos tener en el código cosas 'imposibles': -}

miTuplaLoca = (1/0, "hola")
miTuplaLoca' = ("hola", undefined)

elSegundo = snd miTuplaLoca
elPrimero = fst miTuplaLoca'


-- No rompe, ya que si bien 1/0 no es una operación válida, jamás la evalúa

multiplicar (a,b) = a * b
cuadrado n = n * n 
-- Call by Value --> Lo utilizan los lenguajes de paradigma imperativo

-- Paso 0: multiplicar( (3+2) , (6-3) )
-- Paso 1: multiplicar(  5    , (6-3) )
-- Paso 2: multiplicar(  5    ,   3   )
-- Paso 3:               5    *   3
-- Resultado:                 15

-- Paso 0: cuadrado(3 + 2)
-- Paso 1: cuadrado 5
-- Paso 3: 5 * 5
-- Paso 4: 25

-- Paso 0: doble (doble 2)
-- Paso 1: doble (2 + 2)
-- Paso 2: doble (  4  )
-- Paso 3: 4 + 4
-- Paso 4:   8

-- Call by name --> Haskell --> Resuelve de afuera hacia adentro

-- Paso 0: multiplicar( (3+2) , (6-3) )
-- Paso 1:       (3+2)   *  (6-3)
-- Paso 2:         5     *  (6-3)
-- Paso 3:         5     *    3
-- Resultado:            15 

-- Paso 0: cuadrado(3 + 2)
-- Paso 1: (3 + 2) * (3 + 2) --> Expresión repetida
-- Paso 1.5:  b    *    b    --> Sharing: b = puntero a la expresión, lo evalúa una sola vez
-- Paso 3:    5    *    5
-- Paso 4:         25

-- Paso 0: doble (doble 2)
-- Paso 1: doble 2 + doble 2
-- Paso 2: 2 + 2   + doble 2
-- Paso 3: 2 + 2   +   2 + 2
-- Paso 4:   4     +   2 + 2
-- Paso 5:   4     +     4
-- Resultado:      8 

-- Usando sharing:

-- Paso 0: doble(doble 2)
-- Paso 1: doble 2 + doble 2
-- Paso 2:   2 + 2 + 2 + 2
-- Paso 3:     4   +   4
-- Resultado:      8


-- Eager Evaluation = Call by value = Evaluación ansiosa
-- Lazy evaluation existe porque existe la transparencia referencial, evaluar antes o despues da igual.


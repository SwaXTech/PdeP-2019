-- Clase #4 10/04/2019 --
{-

Repaso:

-Currificación: Sucesión de funciónes que esperan un solo parámetro formando otra función que espera el siguiente parámetro.

No se puede usar la aplicación parcial sin el concepto de currificación en el lenguaje.
Usamos funciones parcialmente aplicadas para posteriormente poder componerlas.

-Función de orden superior: Recibe como parámetro otra función. Ej: la composición.
-}

-- En matemáticas tenemos las llamadas 'funciónes partidas'. Un concepto similar existe en Haskell y se llaman GUARDAS:

modulo unNumero
    | unNumero >= 0 = unNumero
    | unNumero < 0  = -unNumero

-- | <-- Este símbolo se llama pipeline y nos sirve para separar los casos.

-- Main> modulo 5
-- 5

-- Main> modulo (-5)
-- 5


{-
Analicemos el caso en el que tenemos que averiguar el precio de una entrada segun si es mayor de edad, menor de edad o jubilado.
-}

esMayorDeEdad :: (Num a, Ord a) => a -> Bool
esMayorDeEdad unaEdad = unaEdad >= 18

precioEntrada unaEdad
    | unaEdad < 18 = 160
    | esMayorDeEdad unaEdad = 250
    | unaEdad > 60 = 180

{-Esta función tiene un problema. Los casos se leen desde arriba hacia abajo, lo que significa que si una de las condiciónes se cumple
entonces las posteriores no se evalúan.
En este caso, todo el 'dominio' se evalúa hasta la segunda condición, por lo tanto, la tercera nunca será tomada en cuenta.
La correjimos:
-}

precioEntrada' unaEdad
    | unaEdad < 18  = 160
    | unaEdad >= 60 = 180
    | esMayorDeEdad unaEdad = 250


-- Al tener definida la función esMayorDeEdad, quizá sea conveniente usarla en todos los casos donde sea posible:

precioEntrada'' unaEdad
    | not (esMayorDeEdad  unaEdad) = 160
    | unaEdad >= 65 = 180
    | esMayorDeEdad unaEdad = 250
    

-- ¿Qué pasa si ingreso una edad que no tenemos contemplada?, por ejemplo (-5).
-- El programa nos tiene en cuenta como si fuesemos menores de edad, podemos solucionar agregando una condición

precioEntrada''' unaEdad
    | unaEdad > 0 && unaEdad < 18 = 160
    | unaEdad >= 65 = 180
    | esMayorDeEdad unaEdad = 250


-- En esta función ya contemplamos que la edad debe ser positiva, el problema es que al ingresar un valor negativo
-- el programa detiene su ejecución, lo cual es razonable ya que no es posible que existan edades negativas.

-- Quizá nos veamos tentados a agregar un último caso: unaEdad < 0 = "No podés pasar"
-- Esa línea de código impide que podamos ejecutar ya que el TIPO DE RETORNO DE UNA FUNCIÓN ES UNICO.


{-
Observación:

Del lado izquierdo del '=' tenemos expresiones Booleanas,
por lo tanto es posible que esta expresión sea 'True' por ejemplo,
con el objetivo de evaluar los casos no contemplados en las condiciones anteriores.
Para esto usamoas 'otherwise' que equivale a True

> otherwise
True
-}

--Calculamos un descuento al precio de la entrada según la cantidad de letras que tenga el nombre, en el caso de que no sea ni menor de edad ni jubilado:

precioEntradaConDescuento unaEdad unNombre
    | unaEdad < 18 = 160
    | unaEdad > 65 = 180
    | otherwise = 250 - length unNombre

{-
Observación:
Si nosotros queremos restar por ejemplo 250.32 - (length "hola") el programa termina su ejecución, aunque está permitido su uso en el parcial.
Esto se debe a que 250.32 es un Fractional (Float o Duble) y le estamos restando un Int, que viene dado por el Tipo de la función length
-}


-- Podemos pensar el uso de guardas como una representación de una función partida como concepto matemático

funcionPartida unNumero
    | unNumero >= 1 || unNumero <= (-1) = 7
    | otherwise = 5


-------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Necesitamos una función que me diga el gusto de helado favorito según la persona, usamos, por ejemplo, el concepto de guardas:


gustoFavorito :: String -> String
gustoFavorito unNombre 
   | unNombre == "Martín"  = "Banana Split"
   | unNombre == "Ignacio" = "Chocolate"
   | unNombre == "Fede"    = "Vainilla"

-- ¿Funciona? Si... Pero, hay un problema, tenemos lógica repetida. En cada uno de los casos se emplea la misma expresión lógica que podemos reducir usando un nuevo concepto:

--PATTERN MATCHING--

gustoFavorito' "Martín"  = "Banana Split"
gustoFavorito' "Ignacio" = "Chocolate"
gustoFavorito' "Fede"    = "Vainilla"
gustoFavorito' _          = "Dulce de leche" -- El guión bajo lo utilizamos para representar que el parámetro no nos importa --

-- En el Pattern Matching diferenciamos 3 conjuntos:

-- Patrones --> Cosas que se comparan por igual (Ej; "Martín") --> Izquierda del igual
-- Valores  --> Ej: "Banana Split"                             --> Derecha del igual
-- Tipos    --> Ej: String

-- IMPORTANTE: JAMÁS EL GUIÓN BAJO VA A LA DERECHA --

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Ejemplos de funciones que dan mucha fiaca escribir:

sumarTres num1 num2 num3 = num1 + num2 + num3
sumarCinco num1 num2 num3 num4 num5 = num1 + num2 + num3 + num4 + num5

--sumarDoscientosCincuenta???????


--Usamos Listas: Conjunto ordenado de valores del mismo tipo.

-- Ordenado --> [3, 2, 1] == [1, 2, 3] --> False


--Ejemplos:

sumar :: Num a => [a] -> a
sumar unosNumeros = sum unosNumeros

{-
> sumar [1..10]
55
-}

{-
Observación:
[Int] --> Una lista de enteros
[Char] -> Un String
-}

{-
>['a'..'z']
"abcdefghijklmnopqrstuvwxyz"
-}

-- Algunas funciónes: (Para mejor compresión se consideran otros tipos para algunas funciónes: Ej: length, sum, product)

-- Index of:
-- (!!) :: [a] -> Int -> a 
-- Ej: [1..10] !! 9

-- Longitud:
-- length :: Foldable t => t a -> Int
-- length :: [a] -> Int

-- Sumatoria:
-- sum :: Num a => [a] -> a

-- Productoria:
-- product :: Num a => [a] -> a

-- Si el elemento pertenece a la lista:
-- elem :: Eq a => a -> [a] -> Bool

-- Concatenar al principio | Agregar head:
-- (:) :: a -> [a] -> a

-- Cabeza de lista | 1° Elemento:
-- head :: [a] -> a

-- Cola de lista | Elementos después de head:
-- tail :: [a] -> [a]

-- Concatenación:
-- (++) :: [a] -> [a] -> [a]

-- Invertir:
-- reverse :: [a] -> [a]

-- Tomar los primeros n elmentos
-- take :: Int -> [a] -> [a]

-- Tomar los últimos n elementos
-- drop :: Int -> [a] -> [a]

-- Último elemento:
-- last :: [a] -> a

-- Todos menos el último:
-- init :: [a] -> [a]

-- Máximo:
-- maximum :: Ord a => [a] -> a

-- Mínimo
-- minimum :: Ord a => [a] -> a

-- Cadena vacía:
-- null :: [a] -> Bool



-- En la primera clase vimos una función que nos devuelve el doble de los pares, es momento de usarla:

elDobleDeLosPares numeros = (map (*2) . filter even) numeros

-- Función map --
-- Es una función de orden superior
-- map :: (a -> b) -> [a] -> [b]
-- Reliza la función especificada a la lista
-- map (*2) [1..10] multiplica por dos cada uno de los números de la lista.


-- Función filter --
-- Es una función de orden superior
-- Recibe como parámetro una función cuyo valor de retorno es un Booleano
-- Filtra la lista con el criterio de la función pasada por parámetro
-- filter even [1..10] devuelve una lista con los que cumplen que la función even de True, o sea los pares.
-- filter :: (a -> Bool) -> [a] -> [a]


-- Función all --
-- Es una función de orden superior
-- Recibe como parámetro una función cuyo valor de retorno es un Bool
-- Devuelve True/False según si la lista cumple que TODOS sus elementos devuelven True en la función pasada por parámetro
-- > all (< 21) [20..30]
-- False --> Hay valores en la lista que no cumplen con ser menores que 21
-- all :: (a -> Bool) -> [a] -> Bool


-- Función any --
-- Es una función de orden superior
-- Recibe como parámetro una función cuyo valor de retorno es un Bool
-- Devuelve True/False según si la lista cumple que AL MENOS UNO de sus elementos devuelve True en la función pasada por parámetro
-- any :: (a -> Bool) -> [a] -> Bool


-- Función concat --
-- Concatena las listas contenidas dentro de una lista en una sola lista:
-- Ej: >concat [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- [1, 2, 3, 4, 5, 6, 7, 8, 9]
-- concat :: [[a]] -> [a]

-- Función replicate--
-- Replica n veces un elemento
-- >replicate 10 4
-- 4444444444

-- Función zipWith
-- Función que 'aparea' los valores de uno en uno según la función que recibe por parámetro
-- zipWith :: (b -> c -> a) -> [b] -> [c] -> [a]
-- Ej: zipWith (+) [1..10] [20..30]
-- [1+20, 2+21, 3+22...]

-- Funciones Locas
-- >concat $ map (++[10]) [[1,2,3,4], [5,6,7]]
-- [1,2,3,4,10,5,6,7,10]

-- zipWith ($) [even, odd, even] [1..3]
-- [False,False,False]








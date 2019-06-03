-- Clase #2 27/03/2019 --
{-
- Declaratividad  --> Pensar en el qué y no en el cómo
- Expresividad    --> Nombres que describen lo que representan
- Funciones       --> Se operan funciónes
- Valores        
- Inmutabilidad   --> Las variables son inmutables
- Efecto          --> Las funciónes no tienen efecto en las variables.
- Tipado          --> Realizamos la firma de cada función
- Clases de tipos --> Engloban tipos.

-}

--sumatoria unosNumeros = foldl(+) unosNumeros

concatenacion = "hola, " ++ "como estas?"

-- Funciónes --

devuelveSiEsPar  x = even x
devuelveSiEsImpar x = odd x
devuelveMaximo x y = max x y
devuelveMinimo x y = min x y
devuelveLongitud str = length str
devuelveLoMismo x = id x
devuelveElPrimero x y = const x y
devuelveElUltimo x y = seq x y

funcionEven = even
funcionEven' = (id even)


-- Parametros que se pueden prescindir --

doble num = num * 2
sgte  num = num + 1

dobleDelSgte num = (doble . sgte) num
dobleDelSgte' = doble . sgte


-- Chirimbolos --
esCincoPar = even $ 5

-- Notación --
inFija  = 123 + 456 --  Naturalmente infija
preFija = (+) 123 456

esMultiploDe mult div = mod div mult == 0 -- Naturalmente prefija
-- > esMultiploDe 4 8
-- > 4 `esMultiploDe` 8 


-- Inmutabilidad --

miBilletera = 100

comprarUnAgua unaBilletera = unaBilletera - 30 -- No cambia el valor de unaBilletera

miBilleteraDespuesDeComprarUnAgua = comprarUnAgua miBilletera

miBilleteraDespuesDeComprarTresAguas unaBilletera  = (comprarUnAgua . comprarUnAgua . comprarUnAgua) unaBilletera
miBilleteraDespuesDeComprarTresAguas' unaBilletera = comprarUnAgua . comprarUnAgua . comprarUnAgua $ unaBilletera
miBilleteraDespuesDeComprarTresAguas'' unaBilletera = comprarUnAgua . comprarUnAgua $ comprarUnAgua unaBilletera


{-
Las operaciónes (., $, etc) tienen jerarquía 

- Paréntesis
- Aplicación normal
- Punto (.) -> Composición
- Potencia (^)
- Mult-Div (*, /)
- Sum-Rest (+, -)
- Comparación (==, /=, <, <=, >, >=)
- Ampersand (&&) -> y lógico 
- Barritas (||)  -> o lógico
- Pesos ($)
-}

-- Firma de las funciónes y declaración de tipos / clase de tipos --
    --  Cantidad de parámetros = cantidad de flechitas  --
    -- "Entiendo a las funciónes a partír de sus tipos  --

suma :: Int -> Int -> Int
suma numero otroNumero = numero + otroNumero

doble' :: Int -> Int
doble' x = x * 2

esPar :: Int -> Bool
esPar = even

long :: String -> Int
long = length

yLogico :: Bool -> Bool -> Bool
yLogico = (&&)

yLogico' :: Bool -> Bool -> Bool
yLogico' unBool otroBool = unBool && otroBool

identidad:: a -> a 
identidad = id

constante :: a -> b -> a 
constante = const


-- Para poder usar las funciónes de manera genérica usamos las clases de tipos --

suma' :: Num a => a -> a -> a
suma = (+)

mayor_igual :: Ord a => a -> a -> Bool
mayor_igual = (>=)

igual_igual :: Eq a -> a -> a -> Bool
igual_igual = (==)

{-

    a -> b
|-----------------------------------------------|
|   -Show-                                      |
|  |-----------------------------------------|  |
|  |    -Eq-   Bool                          |  |
|  | |------------------------------------|  |  |
|  | |   -Ord-                            |  |  |
|  | |  |-------------------------------| |  |  |
|  | |  |  Char String                  | |  |  |
|  | |  |     -Num-                     | |  |  |
|  | |  | |---------------------------| | |  |  |
|  | |  | |  -Integral-  -Fraccional- | | |  |  |
|  | |  | | |---------| |-----------| | | |  |  |
|  | |  | | |Int      | | Float     | | | |  |  |
|  | |  | | |Integer  | | Double    | | | |  |  |
|  | |  | | |---------| |-----------| | | |  |  |
|  | |  | |---------------------------| | |  |  | 
|  | |  |-------------------------------| |  |  |
|  | |------------------------------------|  |  |
|  |-----------------------------------------|  |
|-----------------------------------------------|


Nota: Para saber el tipo de dato que recibe/devuelve una función se pide al intérprete con :t
:t suma
-}










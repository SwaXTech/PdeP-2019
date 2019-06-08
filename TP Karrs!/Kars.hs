                                                                                -- TP Funcional 2019 - Karrrrs! --                              
                                                                                --  Paradigmas de Programación  --  
                                                                                --      Grupo Tom y Jerry       --
                                                                                --         08/05/2019           --                                                               
module Kars where

-- PUNTO 1 --
import Text.Show.Functions

type Truco = (Auto -> Auto)

data Auto = Auto {
    nombre :: String,
    nivelNafta :: Float,
    velocidad :: Float,
    nombreEnamorade :: String,
    truco :: Truco
} deriving Show

-- metrosPista = 1000                       -- Ya no se usa.

deReversa :: Truco -- La función deReversa aumenta en un quinto de la velocidad actual del Auto, el nivel de nafta --
deReversa (Auto nombre nivelDeNafta velocidad nombreEnamorade truco) = Auto nombre (nivelDeNafta + (velocidad / 5)) velocidad nombreEnamorade truco

cambiarVelocidad :: (Float->Float) -> Auto -> Auto --Funcion auxiliar
cambiarVelocidad funcion (Auto nombre nivelDeNafta velocidad nombreEnamorade truco) = (Auto nombre nivelDeNafta (funcion velocidad) nombreEnamorade truco)

impresionar :: Truco -- La función impresionar duplica la velocidad del Auto para impresionar a su enamorade --
impresionar = cambiarVelocidad (* 2)

nitro :: Truco
nitro = cambiarVelocidad (+ 15)

fingirAmor :: String -> Truco -- La función fingirAmor elige a otre enamorade por conveniencia --
fingirAmor otroEnamorade (Auto nombre nivelDeNafta velocidad _ truco) = Auto nombre nivelDeNafta velocidad otroEnamorade truco

-- Modelos --
rochaMcQueen  = Auto "RochaMcQueen" 300 0 "Ronco" deReversa
biankerr      = Auto "Biankerr" 500 20 "Tinch" impresionar
gushtav       = Auto "Gushtav" 200 130 "PetiLaLinda" nitro
rodra         = Auto "Rodra" 0 50 "Taisa" (fingirAmor "Petra")


-- PUNTO 2 --

incrementarVelocidad :: Truco -- La función incrementarVelocidad incrementa la velocidad del auto según la cantidad de vocales del nombre de su enamorade --
incrementarVelocidad auto = cambiarVelocidad ((+) . calcularAumento . nombreEnamorade $ auto) auto

-- Función delegada, permite saber cuánto es el aumento de la velocidad según la cantidad de vocales del enamorade --
calcularAumento :: String -> Float
calcularAumento nombreEnamorade
    | cantidadVocales nombreEnamorade >  4  = 30
    | cantidadVocales nombreEnamorade >= 3  = 20
    | cantidadVocales nombreEnamorade >= 1  = 15
    | cantidadVocales nombreEnamorade <  1  = 0



-- Función delegada, permite saber cuántas vocales posee un nombre --
cantidadVocales :: String -> Int
cantidadVocales = length . filter esVocal

-- Función delegada, permite saber si una letra es vocal --
esVocal :: Char -> Bool
esVocal letra = elem letra "aeiou"

-- PUNTO 3 --

puedeRealizarTruco :: Auto -> Bool -- La función puedeRealizarTruco devuelve True si es posible realizar un truco, o sea, si tiene nafta en el tanque y su velocidad es menor a 100 --
puedeRealizarTruco (Auto _ nivelDeNafta velocidad _ _) = nivelDeNafta > 0 && velocidad < 100

-- PUNTO 4 --

comboLoco :: Truco -- La función comboLoco permite realizar reversa con nitro --
comboLoco = deReversa . nitro

queTrucazo :: String -> Truco -- La función queTrucazo primero cambia de enamorade y luego incrementa su velocidad --
queTrucazo otroEnamorade = incrementarVelocidad . fingirAmor otroEnamorade

turbo :: Truco -- La función turbo lleva el nivel de nafta a 0 y aumenta la velocidad en la cantidad de nafta que tenía * 10 --
turbo auto =  cambiarNafta((-) . nivelNafta $ auto) . (cambiarVelocidad ((+) . (* 10) . nivelNafta $ auto)) $ auto

cambiarNafta :: (Float->Float) -> Auto -> Auto --Funcion auxiliar
cambiarNafta funcion (Auto nombre nivelDeNafta velocidad nombreEnamorade truco) 
    | (>= 0) . funcion $ nivelDeNafta = (Auto nombre (funcion nivelDeNafta) velocidad nombreEnamorade truco)
    | otherwise = (Auto nombre 0 velocidad nombreEnamorade truco)

--------------------------------------------------------------------------------------------- ENTREGA 2 -------------------------------------------------------------------------------------
-- PUNTO 1 --

-- Tipo para las funciones que van a ser de trampa, como afecta a los participantes de la carrera recibe esa lista, y devuelve una lista de participantes modificada
type Trampa = [Auto] -> [Auto]

data Carrera = Carrera {

    vueltas :: Int,
    longitud :: Float,
    nombresDelPublico :: [String],
    trampa :: Trampa,
    participantes :: [Auto]

} deriving Show

potreroFunes :: Carrera -- Carrera progrmada con 3 vueltas, de longitud 5.
potreroFunes = Carrera 3 5.0 ["Ronco", "Tinch", "Dodain"] sacarAlPistero [rochaMcQueen, biankerr, gushtav, rodra]


-- PUNTO 2 --

sacarAlPistero :: Trampa -- Elimina al primer participante
sacarAlPistero (x:xs) = xs

lluvia :: Trampa    -- Reduce la velocidad en 10 de todos los participantes
lluvia = map (cambiarVelocidad (+ (- 10)))

inutilidad :: Truco -- Truco para reemplazar con la trampa neutralizarTrucos 
inutilidad = id

neutralizarTrucos :: Trampa -- Elimina la posibilidad de hacer trucos
neutralizarTrucos = map (cambiarTruco inutilidad)

cambiarTruco :: Truco -> Auto -> Auto --Funcion auxiliar, cambia el truco de un Auto
cambiarTruco truco (Auto nombre nivelDeNafta velocidad nombreEnamorade _) = Auto nombre nivelDeNafta velocidad nombreEnamorade truco

pocaReserva :: Trampa -- Los participantes con menos de 30 litros de nafta quedan fuera
pocaReserva = filter ((> 30) . nivelNafta)

podio :: Trampa -- Solo deja seguir compitiendo  los primeros tres participantes de la carrera
podio autos = take 3 autos

-- PUNTO 3 --

consumirNafta :: Float -> Auto -> Auto  -- Resta una determinada cantidad de nafta según la longitud de la pista y la velocidad del participante
consumirNafta longitudCarrera auto = cambiarNafta (flip (-) $ calculoNaftaVuelta longitudCarrera (velocidad auto)) auto

calculoNaftaVuelta :: Float -> Float -> Float -- Calcula cuanta nafta debe consumirse según la velocidad y la longitud de la pista
calculoNaftaVuelta longitud velocidad =  longitud / 10 * velocidad


realizarTruco :: [String] -> Truco -- Realiza el truco si el enamorade está en el público
realizarTruco publico auto
    | puedeRealizarTruco auto && elem (nombreEnamorade auto) publico = truco auto auto
    | otherwise = auto

unaVuelta :: Carrera -> Auto -> Auto -- Modificación correspondiente a un participante por vuelta
unaVuelta carrera auto = (realizarTruco $ nombresDelPublico carrera) . (consumirNafta $ longitud carrera) $ auto

darVuelta :: Carrera -> Carrera -- Corresponde a correr una vuelta más el uso de la trampa
darVuelta carrera = carrera{
    participantes = (trampa carrera) $ map (unaVuelta carrera) (participantes carrera)
}

correrCarrera :: Carrera -> Carrera -- Ejecuta la carrera con infinitas vueltas y devuelve solo el necesario según se especifica en el modelo de la carrera
correrCarrera carrera =  iterate darVuelta carrera !! (vueltas carrera) -- Eventualmente se podría restar uno ya que se indexa desde 0

-- PUNTO 4 --

vaMasRapido :: Auto -> Auto -> Auto -- Devuelve el auto con mayor velocidad entre dos
vaMasRapido auto1 auto2 
    | velocidad auto1 > velocidad auto2 = auto1
    | otherwise = auto2

elMasRapido :: [Auto] -> Auto -- Devuelve el auto con mayor velocidad entre los participantes
elMasRapido participantes = foldr1 vaMasRapido participantes

quienGana :: Carrera -> Auto -- Devuelve el más rápido de la carrera
quienGana carrera = elMasRapido $ participantes.correrCarrera $ carrera

-- PUNTO 5 --

elGranTruco :: [Truco] -> Auto -> Auto -- Aplica todos los trucos en un auto
elGranTruco [] = id
elGranTruco (x:xs) = elGranTruco xs . x 

-- PUNTO 6 --
{-
Luego tenemos la carrera ultra suprema de las altas ligas que tiene una cantidad infinita de participantes! 
De ella queremos saber:
a) ¿Podemos correrla? 
b) ¿Podemos conocer el primer participante luego de 2 vueltas? 
c) ¿Podemos dar la primera vuelta de la carrera? 

RESPUESTAS
a) No se puede correr porque al hacerlo se necesita modificar cada participante, la carrera comienza a ejecutarse pero al ser infinitos no terminaría nunca.
b) Se puede conocer el primer participante luego de dos vueltas, ya que gracias a la evaluación perezosa, no se necesitan evaluar los infinitos participantes de la carrera.
c) No se puede, la vuelta comenzará a ejecutarse, mas nunca terminaría.
-}

carreraInfinita = Carrera 10 5.0 ["mickey", "minnie", "pluto"] sacarAlPistero (iterate id rochaMcQueen)
prueba   = correrCarrera carreraInfinita
prueba'  = head . participantes . darVuelta . darVuelta $ carreraInfinita
prueba'' = darVuelta carreraInfinita


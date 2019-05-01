-- PUNTO 1

data Auto = Auto {
    nombre :: String,
    nivelNafta :: Int,
    velocidad :: Int,
    nombreEnamorade :: String,
    truco :: Auto -> Auto
}

metrosPista = 1000

deReversa :: Auto -> Auto
deReversa (Auto nom nivNaf vel nomEna truc) = Auto nom (nivNaf + (metrosPista `div` 5)) vel nomEna truc

{- subirNafta :: Int -> Float
subirNafta metros = metros / 5 -}

impresionar :: Auto -> Auto
impresionar (Auto nom nivNaf vel nomEna truc) = Auto nom nivNaf (vel * 2) nomEna truc

nitro :: Auto -> Auto
nitro (Auto nom nivNaf vel nomEna truc) = Auto nom nivNaf (vel + 15) nomEna truc

fingirAmor :: String -> Auto -> Auto
fingirAmor otroEnamorade (Auto nom nivNaf vel _ truc) = Auto nom nivNaf vel otroEnamorade truc

rochaMcQueen = Auto "RochaMcQueen" 300 0 "Ronco" deReversa
biankerr = Auto "Biankerr" 500 20 "Tinch" impresionar
gushtav = Auto "Gushtav" 200 130 "PetiLaLinda" nitro
rodra = Auto "Rodra" 0 50 "Taisa" (fingirAmor "Petra")

-- PUNTO 2

incrementarVelocidad :: Auto -> Auto
incrementarVelocidad (Auto nom nivNaf vel nomEna truc) = Auto nom nivNaf (vel + (calcularAumento nomEna)) nomEna truc

calcularAumento :: String -> Int
calcularAumento nomEna
    | cantidadVocales nomEna < 1 = 0
    | cantidadVocales nomEna == 1 || cantidadVocales nomEna == 2 = 15
    | cantidadVocales nomEna == 3 || cantidadVocales nomEna == 4 = 20
    | cantidadVocales nomEna > 4 = 30

cantidadVocales :: String -> Int
cantidadVocales palabra = (length.filter esVocal) palabra

esVocal :: Char -> Bool
esVocal letra = elem letra ['a', 'e', 'i', 'o', 'u']

-- PUNTO 3

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco (Auto _ nivNaf vel _ _) = nivNaf > 0 && vel < 100

-- PUNTO 4

comboLoco :: Auto -> Auto
comboLoco = (deReversa.nitro)

queTrucazo :: String -> Auto -> Auto
queTrucazo otroEnamorade = (incrementarVelocidad.(fingirAmor otroEnamorade))

turbo :: Auto -> Auto
turbo (Auto nom nivNaf vel nomEna truc) = Auto nom 0 (vel + 10 * nivNaf) nomEna truc
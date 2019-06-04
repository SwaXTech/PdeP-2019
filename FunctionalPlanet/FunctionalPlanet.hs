import Text.Show.Functions
type Truco = (Mascota -> Mascota)

data Mascota = Mascota{
    nombre :: String,
    edad :: Int,
    suDueño :: Dueño,
    nivelDeEnergia :: Int,
    estaDistraida :: Bool,
    trucos :: [Truco]
} deriving (Show)

data Dueño = Dueño{
    nombreDueño :: String,
    añosDeExperiencia :: Int
} deriving(Show)

-- Punto 1 --

ayudanteDeSanta = Mascota {
    nombre = "Ayudante de Santa",
    edad = 10,
    suDueño = (Dueño "Bart Simpson" 5),
    nivelDeEnergia = 50,
    estaDistraida = False,
    trucos = [sentarse, hacerseElMuerto, tomarAgua, mortalTriple]
}

bolt = Mascota {
    nombre = "Bolt",
    edad = 5,
    suDueño = (Dueño "Penny" 1),
    nivelDeEnergia = 100,
    estaDistraida = False,
    trucos = [perroMojado, hacerseElMuerto, sentarse, mortalTriple]
}

laTortuga = Mascota {
    nombre = "La Tortuga",
    edad = 32,
    suDueño = (Dueño "Fede Scarpa" 30),
    nivelDeEnergia = 30,
    estaDistraida = True,
    trucos = [sentarse, sentarse, sentarse, tomarAgua]
}

-- Punto 2 --
sentarse :: Truco
sentarse mascota = consumirPuntos 5 mascota

tomarAgua :: Truco
tomarAgua mascota = recuperarPuntos 5 mascota

perroMojado :: Truco
perroMojado mascota = (consumirPuntos 5) . (cambiarNombre ("Pobre " ++ (nombre mascota))) $ mascota

hacerseElMuerto :: Truco
hacerseElMuerto mascota = (modificarDistraccion True) . (recuperarPuntos 10) $ mascota

mortalTriple :: Truco
mortalTriple mascota = (consumirPuntos 20) . (sumarAños 10) $ mascota

consumirPuntos :: Int -> Truco
consumirPuntos n mascota = modificarEnergia (-) n mascota

recuperarPuntos :: Int -> Truco
recuperarPuntos n mascota = modificarEnergia (+) n mascota

modificarEnergia :: (Int -> Int -> Int) -> Int -> Mascota -> Mascota
modificarEnergia f n mascota = mascota {
    nivelDeEnergia = (nivelDeEnergia mascota) `f` n
}

sumarAños :: Int -> Mascota -> Mascota
sumarAños n mascota = mascota{
    suDueño = Dueño (nombreDueño . suDueño $ mascota) ((+n) . añosDeExperiencia . suDueño $ mascota)
}

cambiarNombre :: String -> Mascota -> Mascota
cambiarNombre nuevoNombre mascota = mascota {
    nombre = nuevoNombre
}

modificarDistraccion :: Bool -> Mascota -> Mascota
modificarDistraccion estado mascota = mascota{
    estaDistraida = estado
}

-- Punto 3 --

realizarPresentacion :: Mascota -> Mascota
realizarPresentacion mascota = realizarTrucos mascota (trucos mascota)

realizarTrucos :: Mascota -> [Truco] -> Mascota
realizarTrucos mascota [] = mascota
realizarTrucos mascota (x:xs)
    | puedeRealizarTruco mascota = realizarTrucos (x $ mascota) xs
    | otherwise = realizarTrucos(modificarDistraccion False mascota) xs

puedeRealizarTruco :: Mascota -> Bool
puedeRealizarTruco mascota = (not . estaDistraida $ mascota) && (not . estaCansada $ mascota)

estaCansada mascota = (<= 0) . nivelDeEnergia $ mascota

-- Punto 4 --

type Resultado = (String, Int, Int, Int)
type Puntaje = (Mascota -> Int)

resultados :: Mascota -> Resultado
resultados mascota = (nombre mascota, puntosDeEnergia mascota, puntosDeHabilidad mascota, puntosDeTernura mascota)

puntosDeEnergia :: Puntaje
puntosDeEnergia mascota = (*(edad mascota)). nivelDeEnergia . realizarPresentacion $ mascota

puntosDeHabilidad :: Puntaje
puntosDeHabilidad mascota = (length. trucos $ mascota) * (añosDeExperiencia . suDueño $ mascota)

puntosDeTernura :: Puntaje
puntosDeTernura mascota 
    | take 5 (nombre mascota) == "Pobre" = 20
    | otherwise = 20 - (edad mascota)

-- Punto 5 --

data Criterio = Habilidad | Energia | Ternura deriving (Show, Eq)

ganadorDeCategoria :: Criterio -> [Mascota] -> String
ganadorDeCategoria Habilidad mascotas = masHabil mascotas
ganadorDeCategoria Energia mascotas = masEnergico mascotas
ganadorDeCategoria Ternura mascotas = masTierno mascotas

masHabil :: [Mascota] -> String
masHabil mascotas = elMejorDe (habilidad) mascotas

masEnergico :: [Mascota] -> String 
masEnergico mascotas = elMejorDe (energia) mascotas

masTierno :: [Mascota] -> String
masTierno mascotas = elMejorDe (ternura) mascotas

elMejorDe :: (Resultado -> Int) -> [Mascota] -> String
elMejorDe f mascotas = nombreMascota $ foldl1 (mejor f) (map (resultados . realizarPresentacion) mascotas)

mejor :: (Resultado -> Int) -> Resultado -> Resultado -> Resultado
mejor f m1 m2 
    | (f m1) >= (f m2) = m1
    | otherwise = m2

nombreMascota :: Resultado -> String 
nombreMascota (nombre, _, _, _) = nombre
habilidad :: Resultado -> Int
habilidad (_, _ , puntosHabilidad, _) = puntosHabilidad
energia :: Resultado -> Int
energia (_, energ, _, _) = energ
ternura :: Resultado -> Int
ternura (_,_,_, ter) = ter

-- Punto 6 --

ganadorDelConcurso :: [Mascota] -> String
ganadorDelConcurso mascotas = nombreMascota $ foldl1 (masPuntaje) (map (resultados. realizarPresentacion) mascotas)

masPuntaje r1 r2 
    | sumaDeTuplas r1 >= sumaDeTuplas r2 = r1
    | otherwise = r2

sumaDeTuplas (_, v1, v2, v3) = v1 + v2 + v3


-- Punto 7 --

promedioDePuntos mascotas = (sum (map (sumaDeTuplas) (map (resultados . realizarPresentacion) mascotas))) `div` fromIntegral (length mascotas)








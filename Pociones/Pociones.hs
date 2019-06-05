
aplicar3 f (a, b, c) = (f a, f b, f c)

-- Resolución --

type Persona = (String, Niveles)
type Niveles = (Int, Int, Int)
type Efecto = (Persona -> Persona)

data Pocion = Pocion{
    nombre :: String,
    ingredientes :: [Ingrediente]
}

data Ingrediente = Ingrediente{
    nombreIngrediente :: String,
    cantidad :: Int,
    efectos :: [Efecto]
}

sumaNiveles :: Niveles -> Int
sumaNiveles (v1,v2,v3) = v1 + v2 + v3

diferenciaNiveles :: Niveles -> Int
diferenciaNiveles (v1,v2,v3) = (max v1 (max v2 v3)) - (min v1 (min v2 v3))

sumaNivelesPersona :: Persona -> Int
sumaNivelesPersona persona = sumaNiveles . snd $ persona

diferenciaNivelesPersona :: Persona -> Int
diferenciaNivelesPersona persona = diferenciaNiveles . snd $ persona

efectosDePocion :: Pocion -> [Efecto]
efectosDePocion pocion = concatMap (efectos) (ingredientes pocion)

pocionesHeavies :: [Pocion] -> [String]
pocionesHeavies pociones = map (nombre) (pocionesConMasDeCuatroEfectos pociones)

pocionesConMasDeCuatroEfectos :: [Pocion] -> [Pocion]
pocionesConMasDeCuatroEfectos pociones = filter ((>=4) . length . efectosDePocion) pociones

pertenece [] a = False
pertenece (x:xs) a = a == x || pertenece xs a 

incluyeA :: Eq a => [a] -> [a] -> Bool
incluyeA lista1 lista2 = all (== True) (map (pertenece lista2) lista1) 

esPocionMagica :: Pocion -> Bool
esPocionMagica pocion = (algunoTieneTodasLasVocales . ingredientes $ pocion) && (todosTienenGramosPar . ingredientes $ pocion)

algunoTieneTodasLasVocales :: [Ingrediente] -> Bool
algunoTieneTodasLasVocales ingredientes = (>0) . length $ filter (tieneTodasVocales.nombreIngrediente) ingredientes

tieneTodasVocales :: String -> Bool
tieneTodasVocales palabra = incluyeA "aeiou" palabra

todosTienenGramosPar :: [Ingrediente] -> Bool
todosTienenGramosPar ing = all (tieneGramosPar) ing

tieneGramosPar :: Ingrediente -> Bool
tieneGramosPar ing = even . cantidad $ ing

tomarPocion :: Pocion -> Persona -> Persona
tomarPocion pocion persona = foldl (flip ($)) persona (efectosDePocion pocion)

esAntidoto :: Persona -> Pocion -> Pocion -> Bool
esAntidoto persona pocion1 pocion2 = (== persona). (tomarPocion pocion2) . (tomarPocion pocion2) $ persona

personaMasAfectada :: Pocion -> (Niveles -> Int) -> [Persona] -> Persona
personaMasAfectada pocion f personas = masAfectada (f) (map (tomarPocion pocion) personas)

masAfectada :: (Niveles -> Int) -> [Persona] -> Persona
masAfectada f personas = foldl1 (masAfectadaDeDos f) personas

masAfectadaDeDos :: (Niveles -> Int) -> Persona -> Persona -> Persona
masAfectadaDeDos f persona1 persona2 
    | (f . snd $ persona1) >= (f . snd $ persona2) = persona1
    | otherwise = persona2


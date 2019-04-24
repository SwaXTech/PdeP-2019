type Pelicula = (String, [Int]) -- De la película sabemos su título y un conjunto de calificaciónes

-- Por Expresividad:

nombre = fst
calificaciones = snd


-- Saber si la película es popular, lo cual ocurre si tiene más de 5 calificaciónes:

esPopular :: Pelicula -> Bool
esPopular pelicula =  (> 5) . length  . calificaciones $ pelicula

-- De una manera más simple:

esPopular' :: (String, [Int]) -> Bool
esPopular' (_, calif) = (>5) . length $ calif


-- Saber si una película esta buena, que pasa si la longitud de su título es mayor a 7 y además es popular
estaBuena :: Pelicula -> Bool
estaBuena pelicula = ((> 7) . length . nombre $ pelicula) && esPopular' pelicula

-- Obtener el puntaje de una película, o sea el promedio de sus calificaciónes:

--puntaje :: (String, [Int]) -> Float
puntaje (_, calif) = (sum calif) / fromIntegral(length calif)

-- Saber la dispersión de una película, o sea la diferencia entre la mejor calificación y la peor
dispersion :: (String, [Int]) -> Int
dispersion (_, calif) = (maximum calif) - (minimum calif)

-- Calificar una película

calificar:: Pelicula  -> Int -> Pelicula
calificar (titulo, calif) nota =  (titulo, nota : calif) -- Devolvemos una nueva película con un mismo título y la misma lista de calificaciones con un nuevo head

-- Manijear una película, es decir, sumarle dos puntos a todas las calificaciones a menos que sea 9,10 o 1

sumarSegun :: Int -> Int
sumarSegun 1 = 1
sumarSegun calificacion = min 10 calificacion

manijear :: Pelicula -> Pelicula
manijear (titulo, calif) = (titulo, map sumarSegun calif)

-- Modelar maratón

type Maraton = [Pelicula]

-- Saber si vale la pena, o sea si la primer y última película están buenas

valeLaPena :: Maraton -> Bool
valeLaPena maraton = (estaBuena . head  $ maraton) && (estaBuena . last $ maraton)


-- Saber si una película pertenece a una maratón

perteneceAMaraton :: Maraton -> Pelicula -> Bool
perteneceAMaraton maraton (titulo, _) = (>0) . length $ filter (== titulo) (map nombre maraton)

-- Saber cuál es la valoración de una maratón, que es el promedio de los puntajes de sus películas:

--puntajesDeLaMaraton :: Maraton -> [Int]
puntajesDeLaMaraton maraton = map puntaje maraton

--valoracion :: Maraton -> Float
valoracion maraton = (sum . puntajesDeLaMaraton $ maraton) / (fromIntegral . length . puntajesDeLaMaraton $ maraton)


--Maratón de Oscar, o sea cuando todas las películas con baja dispersión tienen puntaje superior a 8. La dispersión es baja si es menor que 2

peliculasDeBajaDispersion :: Maraton -> Maraton
peliculasDeBajaDispersion maraton = filter ((< 2). dispersion) maraton

maratonDeOscar :: Maraton -> Bool
maratonDeOscar maraton = all (>= 8) (map puntaje (peliculasDeBajaDispersion maraton))

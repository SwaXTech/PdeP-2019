-- MODELAR --> Representar la realidad mediante un modelo --
-- Podemos modelar por ejemplo usando listas o TUPLAS --
-- Las tuplas son un par o conjunto de valores ordenados que pueden ser de distinto tipo.

buenosAires = ("Buenos Aires", -25, -37.43)

--Funciones asociadas a la tupla
miTupla = (4,5)

primerElementoDeLaTuplaDeDos = fst
segundoElementoDeLaTuplaDeDos = snd

-- Las funciones fst y snd son específicas para tuplas de 2 elementos. Para n-uplas debemos crearlas.
-- fst :: (a, b) -> a
-- snd :: (a, b) -> b

miTuplaDeCinco = ("UTN", "Medrano 951", "Ciudad de Buenos Aires", "6to piso", 618)

primerElementoDeLaTuplaDeCinco :: (a, b, c, d, e) -> a
primerElementoDeLaTuplaDeCinco (a, _, _, _, _) = a 

segundoElementoDeLaTuplaDeCinco :: (a, b, c, d, e) -> b
segundoElementoDeLaTuplaDeCinco (_, b, _, _ ,_) = b


-- Modelamos un jugador de fútbol cuya abstracción la resolvemos en Nombre y Camiseta

messi   = ("Lionel Messi", 10)
ronaldo = ("Cristiano Ronaldo", 7)
sanchez = ("Macarena Sanchez", 17)
armani  = ("Franco Armani", 1)
ibarra  = ("Hugo Ibarra", 4)

-- Todos nuestros jugadores cumplen con la misma cantidad y tipo de elementos, así que podemos definir un tipo para referirnos a ellos --> TypeAlias

type Jugador = ([Char], Int)

kun:: Jugador
kun = ("Sergio Agüero", 16)

-- Podemos tambien hacer un guiño a la expresividad definiendo las siguientes funciones
nombre = fst
dorsal = snd

-- Definimos algunas funciónes útiles con las tuplas

esJugadorDeCampo jugador = not . esArquero

-- Si hacemos la función esArquero con guardas tendríamos lógica repetida.
-- Usamos pattern matching de la siguiente forma:
esArquero :: Jugador -> Bool
esArquero (_, 1)  = True
esArquero (_, 12) = True
esArquero (_, _)  = False

cumpleTareasDefensivas jugador = esDefensor jugador || esArquero jugador
esDefensor (_, dorsal) = dorsal >= 2 && dorsal <= 6


-- Queremos saber si el apellido del jugador es más largo que el nombre. Como el elemento nombre se divide en dos palabras (Nombre y Apellido) separadas por
-- espacio, usamos la funcion words:: String -> [String] que la separa en una lista de palabras, para luego comparar su longitud:

longitudDeLosNombres jugador = map length (words . nombre $ jugador)
apellidoLargo jugador = (last . longitudDeLosNombres $ jugador) > (head . longitudDeLosNombres $ jugador)


-- Si agregamos un parámetro a las tuplas, tenemos un problema porque todas nuestras funciones las definimos con parametros como tuplas de dos elementos.
-- Deberíamos modificar todos los lugares donde aparezca la tupla.
-- En este caso crearé otro typealias

type JugadorConEquipo = (String, Int, String)

messi'  :: JugadorConEquipo
messi'  = ("Lionel Messi", 10, "Newell´s")
armani' :: JugadorConEquipo
armani' = ("Franco Armani", 1, "River Plate")
ibarra' :: JugadorConEquipo
ibarra' = ("Hugo Ibarra", 4, "Boca Juniors")

equipo (_, _, equipo) = equipo

-- Creamos un DreamTeam que sentirá la camiseta si y solo si todos sus jugadores son del mismo equipo

type Equipo = [JugadorConEquipo]

dreamTeam = [messi', armani', ibarra']

sienteLaCamiseta :: Equipo -> String -> Bool
sienteLaCamiseta jugadores club = all (apoyaUnClub club) jugadores

apoyaUnClub :: String -> JugadorConEquipo -> Bool
apoyaUnClub club jugador = equipo jugador == club





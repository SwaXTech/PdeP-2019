-- Clase #6 Parte 1 24/04/2019 --

{-A veces para modelar, las tuplas no son suficientes, y las funciones que creamos no tienen sentido... Para eso creamos nuestros propios tipos-}

data Jugador = Jugador{ -- El constructor, a la derecha del igual, puede llamarse distinto --
    nombre :: String,
    dorsal :: Int,
    equipo :: String
} deriving (Show) -- el Deriving permite que el Jugador pueda ser mostrado en pantalla.

kun = Jugador {
    nombre = "Sergio Agüero",
    dorsal = 10,
    equipo = "Manchester City"
}

-- Al pedirle el tipo a kun me devuelve que es de tipo Jugador --> Probar en consola :t kun
-- Cada uno de los campos del data implica una función para obtener tal valor

nombreDelKun = nombre kun

-- Otra forma de definir un Jugador es la siguiente:
ehhhhh = Jugador "Diego Maradona" 10 "Boca Juniors" -- Ojo, cada valor separado por espacios

{- Diferencia entre Data y Type
Data:
- Es un tipo, tiene un constructor
- Define las funciónes para acceder a sus componentes automáticamente
- Para poder mostrarlo en pantalla se debe agregar Deriving(Show), sólamente si sus componentes se pueden mostrar,
- De la misma manera se puede usar Deriving(Eq) o Deriving(Show, Eq) para que también se pueda comparar

Type:
- No es un tipo, solo es un alias
- Hay que definir las funciónes a mano
- Se puede mostrar sin hacer nada más, si cada uno de los componentes se pueden mostrar
-}


-- Usamos pattern matching --
esCapitan (Jugador _ dorsal _) = dorsal == 10




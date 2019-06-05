import Data.Char
import Text.Show.Functions

data Archivo = Archivo {
    nombre :: String,
    contenido :: String
} deriving (Show, Eq)

unSimulacro :: Archivo
unSimulacro = Archivo "Hit.hs" "esLarga :: [a] -> Bool\nesLarga = (>9) . length"

-- Punto 1 --
tamaño :: Archivo -> Int
tamaño = (*8) . length . contenido

-- Punto 2 --
estaVacio :: Archivo -> Bool
estaVacio = (==0) . tamaño

-- Punto 3 --
cantidadDeLineas :: Archivo -> Int
cantidadDeLineas = length . lines . contenido

-- Punto 4 --

tieneLineasBlancas :: Archivo -> Bool
tieneLineasBlancas archivo = (>0) . length $ (filter (all (isSpace)) (lines . contenido $ archivo))

-- Punto 5 --
esArchivoHaskell :: Archivo -> Bool
esArchivoHaskell = (== ".hs") . reverse . (take 3) . reverse . nombre

-- Punto 6 --

type Modificacion = (Archivo -> Archivo)

renombrar :: String -> Modificacion
renombrar nuevoNombre archivo = archivo{
    nombre = nuevoNombre
}

-- Punto 7 --

agregarLinea :: String -> Int -> Modificacion
agregarLinea linea n archivo = modificarContenido (insertarEn linea n) archivo

insertarEn :: String -> Int -> [String] -> [String]
insertarEn nuevaLinea n lineas = (take (n - 1) lineas) ++ nuevaLinea : (drop n lineas)

-- Punto 8 --

quitarLinea :: Int -> Modificacion
quitarLinea n archivo = modificarContenido(sacar n) archivo

sacar :: Int -> [String] -> [String]
sacar n lineas = (take (n-1) lineas) ++ (drop (n+1) lineas)

-- Punto 9 --

reemplazarLinea :: Int -> String -> Modificacion
reemplazarLinea n linea archivo = (agregarLinea linea n) . (quitarLinea n) $ archivo

-- Punto 10 --

buscarYReemplazar :: String -> String -> Modificacion
buscarYReemplazar buscada nueva archivo = modificarContenido  (cambiarPalabra buscada nueva) archivo

cambiarPalabra :: String -> String -> [String] -> [String]
cambiarPalabra vieja nueva lineas = map (cambiarEnLinea vieja nueva) lineas

cambiarEnLinea :: String -> String -> String -> String
cambiarEnLinea vieja nueva unaLinea = unwords $ map (reemplazar vieja nueva) (words unaLinea)

reemplazar :: String -> String -> String -> String
reemplazar vieja nueva palabra
    | palabra == vieja = nueva
    | otherwise = palabra

-- Punto 11 -- 

wrappear :: Modificacion
wrappear = modificarContenido (map (wrappearLinea 80))

wrappearLinea :: Int -> String -> String
wrappearLinea cantidad linea
  | length linea < cantidad = linea
  | otherwise               = take cantidad linea ++ '\n' : wrappearLinea cantidad (drop cantidad linea)

-- Punto 12 --
esInutil :: ([String] -> [String]) -> Archivo -> Bool
esInutil f archivo = ((modificarContenido f archivo) == archivo)

-- Punto 13 --

aplicarRevision :: [Modificacion] -> Archivo -> Archivo
aplicarRevision revision archivo = foldl (flip ($)) archivo revision


modificarContenido f archivo = archivo{
    contenido = unlines . f . lines . contenido $ archivo
}








-- Un Data puede tener distintos constructores:

data Booleano = True | False

data Grupo = Metal | NoMetal | GasNoble | Halogeno deriving (Show, Eq)

type Componente = (Sustancia, Int)

data Sustancia = Compuesto{
    nombre :: String,
    componentes :: [Componente],
    grupo :: Grupo
} | Elemento {
    nombre :: String,
    simbolo :: String,
    numeroAtomico :: Int,
    grupo :: Grupo
} deriving(Show) -- Vale para todos 

hidrogeno :: Sustancia
hidrogeno = Elemento "Hidrógeno" "H" 1 NoMetal

oxigeno :: Sustancia
oxigeno = Elemento "Oxígeno" "O" 8 NoMetal

agua :: Sustancia
agua = Compuesto "Agua" [(hidrogeno, 2), (oxigeno, 1)] NoMetal

-- Expresiónes Lambda --

-- ¿Cansado de hacer funciónes al pedo? --

losParesYMayoresATres = filter (\n -> even n && n > 3) [1..10]
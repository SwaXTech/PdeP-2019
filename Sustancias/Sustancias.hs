import Text.Show.Functions

data Sustancia = Elemento {
    nombre :: String,
    simboloQuimico :: String,
    numeroAtomico :: Int,
    grupo :: Grupo
}   | Compuesto {
    nombre :: String,
    componentes :: [Componente],
    grupo :: Grupo
} deriving(Show, Eq)

type Componente = (Sustancia, Int)
data Grupo = NoMetal | Metal | GasNoble | Halógeno deriving (Show, Eq)
data Criterio = Electricidad | Calor deriving(Show, Eq)

-- Punto 1--
hidrogeno = Elemento "Hidrógeno" "H" 1 NoMetal
oxigeno   = Elemento "Oxígeno" "O" 8 NoMetal
agua      = Compuesto "Agua" [(hidrogeno, 2), (oxigeno, 1)]

-- Punto 2 --

conduceBien :: Criterio -> Sustancia -> Bool
conduceBien criterio sustancia = conduceBienSegun criterio (grupo sustancia)

conduceBienSegun _ Metal = True
conduceBienSegun Electricidad GasNoble = True
conduceBienSegun Calor Halógeno = True
conduceBienSegun _ _ = False

-- Punto 3 --

nombreDeUnion :: Sustancia -> String
nombreDeUnion sustancia = (nombreSinVocal . nombre $ sustancia) ++  "uro"

nombreSinVocal :: String -> String
nombreSinVocal nom  = reverse . (dropWhile (esVocal)) . reverse $ nom

esVocal :: Char -> Bool
esVocal = flip elem "aeiouAEIOU"

-- Punto 4 --

combinarDosNombres :: Sustancia -> Sustancia -> String
combinarDosNombres s1 s2 = nombreDeUnion s1 ++ nombre s2

combinarDosNombres' :: String -> String -> String
combinarDosNombres' n1 n2 = nombreDeUnion' n1 ++ n2

nombreDeUnion' :: String -> String
nombreDeUnion' nom = (nombreSinVocal nom) ++ "uro"

-- Punto 5 --
mezclar :: [Componente] -> Sustancia
mezclar componentesAMezclar = Compuesto {
    nombre = nombreDeMezcla componentesAMezclar,
    componentes = componentesAMezclar,
    grupo = NoMetal
}

nombreDeMezcla :: [Componente] -> String
nombreDeMezcla componentesAMezclar = foldl1 (combinarDosNombres') (map (nombre.fst) componentesAMezclar)

-- Punto 6 --
-- Solo se puede saber n componentes (no el último), parte del nombre, y su grupo

-- Punto 7 --

formula :: Sustancia -> String
formula (Elemento _ simbolo _ _) = simbolo
formula (Compuesto _ comps _) = representacionDeComponentes comps

representacionDeComponentes :: [Componente] -> String
representacionDeComponentes comps = "(" ++ (concatMap (representacionDeUnComponente) comps) ++ ")"

representacionDeUnComponente :: Componente -> String
representacionDeUnComponente comp 
    | snd comp == 1 = formula . fst $ comp
    | otherwise = (formula . fst $ comp) ++ (show . snd $ comp)




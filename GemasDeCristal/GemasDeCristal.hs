data Aspecto = UnAspecto {
    tipoDeAspecto :: String,
    grado :: Float
} deriving (Show, Eq)
type Situacion = [Aspecto]

mejorAspecto mejor peor = grado mejor < grado peor
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2
buscarAspecto aspectoBuscado = head. filter (mismoAspecto aspectoBuscado)
buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)
reemplazarAspecto aspectoBuscado situacion = 
    aspectoBuscado : (filter (not.mismoAspecto aspectoBuscado) situacion)

-- 22:07

modificarAspecto :: (Float -> Float) -> Aspecto -> Aspecto
modificarAspecto f aspecto = aspecto {
    grado = f (grado aspecto)
}

esMejor :: Situacion -> Situacion -> Bool
esMejor situacion1 situacion2 = (all (== True)) $ zipWith (mejorAspecto) situacion1 situacion2

modificarSituacion :: String -> (Float -> Float) -> Situacion -> Situacion
modificarSituacion  tipoAspectoBuscado f situacion = reemplazarAspecto (modificarAspecto f (buscarAspectoDeTipo tipoAspectoBuscado situacion)) situacion 
type Personalidad = (Situacion -> Situacion)

data Gema = Gema{
    nombre :: String,
    fuerza :: Int,
    personalidad :: Personalidad
}

vidente :: Personalidad
vidente situacion = (modificarSituacion "incertidumbre" (/2)) . modificarSituacion "tension" (subtract 10) $ situacion

relajada ::Float -> Personalidad
relajada relajamiento situacion = (modificarSituacion "tension" (subtract 30)) $ (modificarSituacion "peligro" (subtract relajamiento) situacion)

gemaVidente = Gema "Gema Vidente" 100 vidente
gemaDescuidad = Gema "Gema Descuidada" 100 (relajada 10)
ganaGema :: Gema -> Gema -> Situacion -> Bool
ganaGema gema1 gema2 situacion = (masFuerte gema1 gema2) && (esMejor((personalidad gema1) situacion) ((personalidad gema2) situacion))

masFuerte :: Gema -> Gema -> Bool
masFuerte gema1 gema2 = fuerza gema1 > fuerza gema2

fusion :: Situacion -> Gema -> Gema -> Gema
fusion situacion gema1 gema2 = Gema{
    nombre = nombreNuevaGema gema1 gema2,
    fuerza = nuevaFuerza gema1 gema2 situacion,
    personalidad = nuevaPersonalidad gema1 gema2 

}

nombreNuevaGema gema1 gema2 
    | nombre gema1 == nombre gema2 = nombre gema1
    | otherwise = nombre gema1 ++ " " ++ nombre gema2

nuevaPersonalidad :: Gema -> Gema -> Personalidad
nuevaPersonalidad gema1 gema2 situacion = (personalidad gema1) . (personalidad gema2) $ map (modificarAspecto (subtract 10)) situacion 

nuevaFuerza gema1 gema2 situacion
    | sonCompatibles gema1 gema2 situacion = ((fuerza gema1) + (fuerza gema2)) * 10
    | masFuerte gema1 gema2 = (*7). fuerza $ gema1
    | otherwise = (*7) . fuerza $ gema2

sonCompatibles :: Gema -> Gema -> Situacion -> Bool
sonCompatibles gema1 gema2 situacion = esMejorQueLaFusion gema1 gema2 situacion && esMejorQueLaFusion gema2 gema1 situacion

esMejorQueLaFusion gema1 gema2 situacion = esMejor ((personalidad gema1) . (personalidad gema2) $ situacion) ((personalidad gema1) situacion)

fusionGrupal :: [Gema] -> Situacion -> Gema
fusionGrupal gemas situacion = foldl1 (fusion situacion) gemas






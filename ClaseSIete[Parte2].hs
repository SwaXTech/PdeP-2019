-- Funciónes de Haskell para listas infinitas --

repetirInfinitos :: a -> [a] -- Infinitos de un valor
repetirInfinitos elem = elem : repeat elem

intinitosCeros = repeat 0

iterar :: (a -> a) -> a -> [a] -- El elemento como primer elemento de la lista, y el elemento anterior, con la función aplicada.
iterar f a =  a : iterate f (f a)

iterarPorDos = iterate  (*2) 1
losPrimerosCien = take 100 iterarPorDos

-- [a, f(a), f(f(a))]

ciclo :: [a] -> [a]
ciclo lista = lista ++ ciclo lista
ciclo' lista = concat . repeat $ lista

cuandoLlegamos = cycle ["Cuando", "llegamos?"]
meTenesPodrido = take 20 cuandoLlegamos

replicar :: Int -> a -> [a]
replicar n a  = take n $ repeat a
replicar' n a = take n (repeat a)

cuandoLlegamos' = replicate 20 "Cuando llegamos?"

replicarConPatternMatching 0 _ = []
replicarConPatternMatching n a = a : replicarConPatternMatching (n-1) a










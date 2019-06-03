--- Clase #1 20/03/2019 ---

{-

Paradigmas de programación:
- Paradigma funcional: Pensar en el qué --> Declaratividad
- Paradigma procedural imperativo: Pensar en el cómo

Operamos:
- Números
- Booleanos
- Char
- String
- Complejos
- Funciónes x -> y
- ...

Aplicamos valores a funciónes 
-}

doble numero = numero * 2
siguiente numero = numero + 1
anterior numero = numero - 1

esMultiplo mult div = mod mult div == 0

dobleDelSgte numero = doble(siguiente numero)
dobleDelSgte' numero = (doble . siguiente) numero
dobleDelSgte'' = doble . siguiente







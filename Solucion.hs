-- Los del codigo: Leandro Flores leannicolasflores@gmail.com 37277904, 
module Solucion where

type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)

type AgenciaDeViajes = [Vuelo]

-- EJERCICIO 1
vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True
vuelosValidos (v:vuelos)
    | vueloValido v && not (origenDestinoRepetido v vuelos) = vuelosValidos vuelos
    | otherwise = False
    where
        origenDestinoRepetido :: Vuelo -> AgenciaDeViajes -> Bool
        origenDestinoRepetido _ [] = False
        origenDestinoRepetido (origen1, destino1, duracion1) ((origen2, destino2, _):vuelos)
            | origen1 == origen2 && destino1 == destino2 = True
            | otherwise = origenDestinoRepetido (origen1, destino1, duracion1) vuelos
        
        vueloValido :: Vuelo -> Bool
        vueloValido (origen, destino, duracion)
            | duracion > 0 && origen /= destino = True
            | otherwise = False

-- EJERCICIO 2
ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadas _ _ = ["BsAs"] -- Borrar y escribir el código correcto


-- EJERCICIO 3
modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota _ = [("BsAs","Rosario",9.0)] -- Borrar y escribir el código correcto


-- EJERCICIO 4
ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada _ = "Rosario" -- Borrar y escribir el código correcto


-- EJERCICIO 5
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar vuelos origen destino = True -- Borrar y escribir el código correcto


-- EJERCICIO 6
duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido _ _ _ = 10.0 -- Borrar y escribir el código correcto



-- EJERCICIO 7
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad ->  Bool
puedoVolverAOrigen vuelos origen = True -- Borrar y escribir el código correcto
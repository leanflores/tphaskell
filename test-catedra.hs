import Test.HUnit
import Data.List
import Solucion
-- No está permitido agregar nuevos imports.


runCatedraTests = runTestTT allTests

allTests = test [
    "vuelosValidos" ~: testsEjvuelosValidos,
    "ciudadesConectadas" ~: testsEjciudadesConectadas,
    "modernizarFlota" ~: testsEjmodernizarFlota,
    "ciudadMasConectada" ~: testsEjciudadMasConectada,
    "sePuedeLlegar" ~: testsEjsePuedeLlegar,
    "duracionDelCaminoMasRapido" ~: testsEjduracionDelCaminoMasRapido,
    "puedoVolverAOrigen" ~: testsEjpuedoVolverAOrigen
    ]

-- corregir los tests si es necesario con las funciones extras que se encuentran al final del archivo

testsEjvuelosValidos = test [
    -- Pruebas básicas de validez
    "vuelos válido con un elemento" ~: vuelosValidos [("BsAs", "Rosario", 5.0)] ~?= True,
    "sin vuelos" ~: vuelosValidos [] ~?= True,

    -- Pruebas de un solo vuelo que no son válidos
    "vuelo no válido con igual origen y destino" ~: vuelosValidos [("BsAs", "BsAs", 5.0)] ~?= False,
    "vuelo no válido con duracion nula" ~: vuelosValidos [("BsAs", "Rosario", 0)] ~?= False,
    "vuelo no válido con duracion negativa" ~: vuelosValidos [("BsAs", "Rosario", (-1))] ~?= False,

    -- Pruebas de vuelos con ciudades repetidas
    "2 vuelos con repeticion" ~: vuelosValidos [("BsAs", "Rosario", 1), ("BsAs", "Rosario", 1)] ~?= False,
    "2 vuelos con distinta duración" ~: vuelosValidos [("BsAs", "Rosario", 1), ("BsAs", "Rosario", 5)] ~?= False,
    "2 vuelos válidos con ciudades invertidas" ~: vuelosValidos [("BsAs", "Rosario", 1), ("Rosario", "BsAs", 1)] ~?= True,
    "3 vuelos válidos con ciudades repetidas entre el primero y ultimo" ~: vuelosValidos [("BsAs", "Rosario", 1), ("Cordoba", "Mendoza", 2), ("BsAs", "Rosario", 2)] ~?= False,

    -- Prueba de combinacion de validos y no validos
    "2 vuelos válidos con todo distinto" ~: vuelosValidos [("BsAs", "Rosario", 1), ("Cordoba", "Mendoza", 2)] ~?= True,
    "un vuelo válido y un vuelo no válido" ~: vuelosValidos [("BsAs", "Rosario", 1), ("Cóordoba", "Mendoza", 0)] ~?= False,
    "3 vuelos válidos todos distintos" ~: vuelosValidos [("BsAs", "Rosario", 1), ("Cordoba", "Mendoza", 2), ("La Plata","Neuquen",6.9)] ~?= True,
    "3 vuelos, 2 validos y 1 no valido" ~: vuelosValidos [("BsAs","Rosario", 3.0),("La Plata","La Plata",0.0),("Rosario","La Plata",7.8)]~?= False
    ]

testsEjciudadesConectadas = test [
    "ciudad conectada con un elemento" ~: ciudadesConectadas  [("BsAs", "Rosario", 5.0)] "Rosario" ~?= ["BsAs"],
    "ciudad sin conectar con un elemento" ~: ciudadesConectadas  [("BsAs", "Rosario", 5.0)] "Cordoba" ~?= [],
    "una ciudad conectada con 2 elementos" ~: ciudadesConectadas  [("BsAs", "Rosario", 5.0), ("Cordoba", "BsAs", 4.0)] "Rosario" ~?= ["BsAs"],
    "ciudad conectada con repetición" ~: ciudadesConectadas  [("BsAs", "Rosario", 5.0), ("Rosario", "BsAs", 4.0)] "Rosario" ~?= ["BsAs"],
    "2 ciudades conectadas de 2 vuelos" ~: expectPermutacion (ciudadesConectadas  [("BsAs", "Rosario", 5.0), ("Cordoba", "Rosario", 4.0)] "Rosario") ["BsAs", "Cordoba"]
    ]

testsEjmodernizarFlota = test [
    "flota modernizada con un elemento" ~: modernizarFlota [("BsAs", "Rosario", 10.0)] ~?= [("BsAs", "Rosario", 9.0)]
    ]

testsEjciudadMasConectada = test [
    "ciudad Mas conectada que aparece dos veces" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "Córdoba", 7.0)] ~?= "Rosario"
    ]

testsEjsePuedeLlegar = test [
    "Se puede llegar caso verdadero con una escala" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ~?= True
    ]

testsEjduracionDelCaminoMasRapido = test [
    "duración del camino más rápido con una escala" ~: duracionDelCaminoMasRapido [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ~?= 10.0
    ]

testsEjpuedoVolverAOrigen = test [
        "puedo volver a origen caso verdadero con una escala" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" ~?= True
    ]



-- Funciones extras

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)
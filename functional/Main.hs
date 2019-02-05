module Main where
import           Arboles
import           Data.List
import           Test.HUnit

--arboles para pruebas:
maderaSolo = Brote Madera
arbol1 = Brote Hoja
arbol2 = Rama Flor (Brote Fruto) (Brote Hoja)
arbol3 = Rama Hoja (Brote Fruto) (Brote Flor)
arbol4 = Rama Madera arbol2 arbol3
arbol5 = Rama Madera arbol3 arbol2
arbol6 = Rama Madera arbol4 arbol1
arbol7 = Rama Madera arbol1 arbol5
arbol8 = Rama Madera arbol1 arbol6
arbol9 = Rama Fruto
    (Rama Fruto (Rama Fruto (Brote Fruto) (Brote Flor)) (Rama Hoja (Brote Madera) (Brote Hoja)))
    (Rama Fruto (Rama Madera (Brote Hoja) (Brote Madera)) (Rama Fruto (Brote Flor) (Brote Fruto)))

soloUnaHoja = Brote Hoja
puroMadera = Rama Madera (Brote Madera) (Brote Madera)
tresFlores = Rama Flor (Brote Flor) (Brote Flor)

maderaYFlor = Rama Madera (Brote Flor) (Brote Madera)
otraFlor = Rama Madera (Brote Madera) (Brote Flor)

frutoSinFlor = Rama Fruto (Brote Madera) (Brote Madera)

protegido = Rama Fruto (Brote Madera) (Brote Flor)
protegidoNivel1Derecha = Rama Madera soloUnaHoja protegido

superProtegido = Rama Hoja protegido protegido

desbalanceado = Rama Fruto (Brote Madera) protegido

-- custom

puroHoja = Rama Hoja (Brote Hoja) (Brote Hoja)
arbolEnUltimaPrimavera = Rama Flor (Brote Fruto) (Brote Flor)

arbol_1a = Rama Madera (Rama Fruto (Brote Flor) (Brote Madera)) (Brote Hoja)
arbol_1b = Rama Madera (Rama Fruto (Brote Hoja) (Brote Hoja)) (Brote Hoja)
arbol_1d = Rama Madera (Rama Fruto (Brote Flor) (Brote Hoja)) (Brote Hoja)

arbol_atacado = Rama Madera (Brote Madera) (Brote Hoja)

maderaHojaMadera = Rama Madera (Brote Hoja) (Brote Madera)

arbolTrunco = Rama Madera (Brote Hoja) (Rama Fruto (Rama Hoja (Rama Fruto (Brote Fruto) (Rama Flor (Brote Fruto) (Brote Madera))) (Rama Fruto (Brote Flor) (Brote Madera))) (Brote Madera))

arbolTruncoAtacadoNivel3Derecho = Rama Madera (Brote Hoja) (Rama Fruto (Rama Hoja (Rama Fruto (Brote Fruto) (Rama Flor (Brote Fruto) (Brote Madera))) (Brote Madera)) (Brote Madera))
arbolTruncoAtacadoNivel3Izquierdo = Rama Madera (Brote Hoja) (Rama Fruto (Rama Hoja (Brote Madera) (Rama Fruto (Brote Flor) (Brote Madera))) (Brote Madera))

arbolTruncoAtacadoNivel4Izquierdo = Rama Madera (Brote Hoja) (Rama Fruto (Rama Hoja (Rama Fruto (Brote Madera) (Rama Flor (Brote Fruto) (Brote Madera))) (Rama Fruto (Brote Flor) (Brote Madera))) (Brote Madera))

arbolTruncoAtacadoNivel5Izquierdo = Rama Madera (Brote Hoja) (Rama Fruto (Rama Hoja (Rama Fruto (Brote Fruto) (Rama Flor (Brote Madera) (Brote Madera))) (Rama Fruto (Brote Flor) (Brote Madera))) (Brote Madera))

puroFruto = Rama Fruto (Brote Fruto) (Brote Fruto)

animalesGolososNivel1 = [(Izquierda, 1, Gula), (Derecha, 1, Gula)]
animalesHambrientosNivel1 = [(Izquierda, 1, Hambre), (Derecha, 1, Hambre)]
animalesFamelicosNivel1 = [(Izquierda, 1, Inanicion), (Derecha, 1, Inanicion)]

hojaMaderaMadera = Rama Hoja (Brote Madera) (Brote Madera)

animalesDesbalanceadosGolosos = [(Derecha, 1, Gula), (Izquierda, 0, Gula)]
animalesDesbalanceadosHambrientos = [(Derecha, 1, Hambre), (Derecha, 0, Hambre)]
animalesDesbalanceadosFamelicos = [(Derecha, 1, Inanicion), (Izquierda, 0, Inanicion)]

animalesDesbalanceadosMixtos = [(Derecha, 1, Inanicion), (Izquierda, 0, Gula)]

animalesTruncoMixtos = [(Izquierda, 5, Gula), (Izquierda, 4, Inanicion), (Izquierda, 3, Gula), (Derecha, 3, Hambre)]

animalesGolososNivel3 = [(Derecha, 3, Gula), (Izquierda, 3, Gula)]

animalesIzquierda = [(Izquierda, 1, Gula), (Izquierda, 1, Hambre)]

--Ejecución de los tests
main :: IO Counts
main = runTestTT allTests

allTests :: Test
allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8
  ]

testsEj2 :: Test
testsEj2 = test [
  0 ~=? peso soloUnaHoja,
  3 ~=? peso puroMadera,
  0 ~=? peso tresFlores,
  2 ~=? peso maderaYFlor,
  2 ~=? peso otraFlor,
  2 ~=? peso frutoSinFlor,
  1 ~=? peso protegido,
  2 ~=? peso protegidoNivel1Derecha,
  2 ~=? peso superProtegido,
  2 ~=? peso desbalanceado,

  0 ~=? perfume soloUnaHoja,
  0 ~=? perfume puroMadera,
  3 ~=? perfume tresFlores,
  1 ~=? perfume maderaYFlor,
  1 ~=? perfume otraFlor,
  0 ~=? perfume frutoSinFlor,
  1 ~=? perfume protegido,
  1 ~=? perfume protegidoNivel1Derecha,
  2 ~=? perfume superProtegido,
  1 ~=? perfume desbalanceado,

  True ~=? puedeVivir soloUnaHoja,
  False ~=? puedeVivir puroMadera,
  False ~=? puedeVivir tresFlores,
  False ~=? puedeVivir maderaYFlor,
  False ~=? puedeVivir otraFlor,
  False ~=? puedeVivir frutoSinFlor,
  False ~=? puedeVivir protegido,
  True ~=? puedeVivir protegidoNivel1Derecha,
  False ~=? puedeVivir superProtegido,
  False ~=? puedeVivir desbalanceado,

  True ~=? mismosComponentes arbol2 arbol3,
  True ~=? mismosComponentes arbol4 arbol5,
  True ~=? mismosComponentes arbol6 arbol7,
  False ~=? mismosComponentes arbol8 arbol9,
  False ~=? mismosComponentes maderaSolo soloUnaHoja,
  False ~=? mismosComponentes puroMadera tresFlores,
  True ~=? mismosComponentes maderaYFlor otraFlor,
  False ~=? mismosComponentes frutoSinFlor otraFlor,
  True ~=? mismosComponentes protegido protegido,
  False ~=? mismosComponentes protegido protegidoNivel1Derecha
  ]

testsEj3 :: Test
testsEj3 = test [
  puroMadera ~=? masPesado [soloUnaHoja, puroMadera, tresFlores, maderaYFlor,
    otraFlor, frutoSinFlor, protegido, protegidoNivel1Derecha, superProtegido,
    desbalanceado, arbol1, arbol2, arbol3, arbol4, arbol5, arbol6, arbol7]
  ]

testsEj4 :: Test
testsEj4 = test [
  arbol9 ~=? crecer id arbol9,
  puroMadera ~=? ultimaPrimavera puroMadera,
  tresFlores ~=? ultimaPrimavera puroHoja,
  arbolEnUltimaPrimavera ~=? ultimaPrimavera arbol2,
  arbolEnUltimaPrimavera ~=? ultimaPrimavera arbol3
  ]

testsEj5 :: Test
testsEj5 = test [
  protegido ~=? comer (Izquierda, 0, Gula) protegido,
  protegido ~=? comer (Izquierda, 0, Hambre) protegido,
  maderaSolo ~=? comer (Izquierda, 0, Inanicion) protegido,

  arbol_atacado ~=? comer(Izquierda, 1, Inanicion) arbol_1a,
  arbol_1b ~=? comer(Izquierda, 1, Gula) arbol_1b,
  arbol_atacado ~=? comer(Izquierda, 1, Hambre) arbol_1b,
  arbol_1d ~=? comer(Izquierda, 1, Hambre) arbol_1d,

  protegidoNivel1Derecha ~=? comer(Derecha, 1, Hambre) protegidoNivel1Derecha,
  maderaHojaMadera ~=? comer(Derecha, 1, Inanicion) protegidoNivel1Derecha,

  superProtegido ~=? comer(Izquierda, 1, Hambre) superProtegido,
  superProtegido ~=? comer(Derecha, 1, Hambre) superProtegido,

  desbalanceado ~=? comer(Derecha, 0, Hambre) desbalanceado,
  maderaSolo ~=? comer(Derecha, 0, Inanicion) desbalanceado,
  desbalanceado ~=? comer(Derecha, 1, Hambre) desbalanceado,
  frutoSinFlor ~=? comer(Derecha, 1, Inanicion) desbalanceado,

  arbolTrunco ~=? comer (Derecha, 1, Gula) arbolTrunco,
  arbolTrunco ~=? comer (Derecha, 1, Hambre) arbolTrunco,
  maderaHojaMadera ~=? comer (Derecha, 1, Inanicion) arbolTrunco,
  arbolTrunco ~=? comer (Derecha, 3, Gula) arbolTrunco,
  arbolTrunco ~=? comer (Derecha, 3, Hambre) arbolTrunco,
  arbolTruncoAtacadoNivel3Derecho ~=? comer (Derecha, 3, Inanicion) arbolTrunco,
  arbolTrunco ~=? comer (Izquierda, 3, Gula) arbolTrunco,
  arbolTrunco ~=? comer (Izquierda, 3, Hambre) arbolTrunco,
  arbolTruncoAtacadoNivel3Izquierdo ~=? comer (Izquierda, 3, Inanicion) arbolTrunco,
  arbolTrunco ~=? comer (Izquierda, 4, Gula) arbolTrunco,
  arbolTruncoAtacadoNivel4Izquierdo ~=? comer (Izquierda, 4, Hambre) arbolTrunco,
  arbolTruncoAtacadoNivel4Izquierdo ~=? comer (Izquierda, 4, Inanicion) arbolTrunco,
  arbolTrunco ~=? comer (Izquierda, 5, Gula) arbolTrunco,
  arbolTruncoAtacadoNivel5Izquierdo ~=? comer (Izquierda, 5, Hambre) arbolTrunco,
  arbolTruncoAtacadoNivel5Izquierdo ~=? comer (Izquierda, 5, Inanicion) arbolTrunco
  ]

testsEj6 :: Test
testsEj6 = test [
  puroFruto ~=? alimentar puroFruto animalesGolososNivel1,
  frutoSinFlor ~=? alimentar puroFruto animalesHambrientosNivel1,
  frutoSinFlor ~=? alimentar puroFruto animalesFamelicosNivel1,
  superProtegido ~=? alimentar superProtegido animalesGolososNivel1,
  superProtegido ~=? alimentar superProtegido animalesHambrientosNivel1,
  hojaMaderaMadera ~=? alimentar superProtegido animalesFamelicosNivel1,
  desbalanceado ~=? alimentar desbalanceado animalesDesbalanceadosGolosos,
  desbalanceado ~=? alimentar desbalanceado animalesDesbalanceadosHambrientos,
  maderaSolo ~=? alimentar desbalanceado animalesDesbalanceadosFamelicos,
  frutoSinFlor ~=? alimentar desbalanceado animalesDesbalanceadosMixtos,
  arbolTruncoAtacadoNivel4Izquierdo ~=? alimentar arbolTrunco animalesTruncoMixtos
  ]

testsEj7 :: Test
testsEj7 = test [
  [arbolTrunco, arbolTruncoAtacadoNivel4Izquierdo] ~=? sobrevivientes animalesGolososNivel3 [arbolTrunco, arbolTruncoAtacadoNivel4Izquierdo],
  [arbol2] ~=? sobrevivientes animalesIzquierda [arbol2, arbol3]
  ]

testsEj8 :: Test
testsEj8 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

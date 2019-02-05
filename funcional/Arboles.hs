module Arboles (Componente(Madera, Hoja, Fruto, Flor), Arbol(Rama, Brote),
 Dirección(Izquierda, Derecha), TipoHambre(Gula, Hambre, Inanicion), Animal,
 foldArbol, peso, perfume, puedeVivir, mismosComponentes, masPesado, crecer,
 ultimaPrimavera, comer, alimentar, sobrevivientes, componentesPorNivel,
 dimensiones) where

data Componente = Madera | Hoja | Fruto | Flor deriving (Eq, Show)

data Arbol = Rama Componente Arbol Arbol | Brote Componente deriving Eq

data Dirección = Izquierda | Derecha deriving Eq

data TipoHambre = Gula | Hambre | Inanicion deriving Eq

type Animal = (Dirección, Int, TipoHambre)

instance Show Arbol where
  show = ("\n" ++) . padArbol 0 0 False

padArbol :: Int -> Int -> Bool -> Arbol -> String
padArbol nivel acum doPad (Brote c) = (if doPad then pad (4*nivel + acum) else "") ++ show c
padArbol nivel acum doPad (Rama x i d) = (if doPad then pad (4*nivel + acum) else "") ++ show x ++
                                          pad 4 ++ padArbol (nivel+1) (acum+l) False i ++ "\n" ++
                                          padArbol (nivel+1) (acum+l) True d where l = length $ show x

pad :: Int -> String
pad i = replicate i ' '

--Ejercicio 1

foldArbol :: (Componente -> b -> b -> b) -> (Componente -> b) -> Arbol -> b
foldArbol fRama fBrote t = case t of Rama c t1 t2 -> fRama c (rec t1) (rec t2)
                                     Brote c      -> fBrote c
                                     where rec = foldArbol fRama fBrote

--Ejercicio 2

incSiIguales :: Componente -> Componente -> Int
incSiIguales c1 c2 | c1 == c2 = 1
incSiIguales _ _   = 0

--- 1
peso :: Arbol -> Int
peso = foldArbol (\c izq der -> izq + der + incSiIguales Madera c)
       (incSiIguales Madera)

--- 2
perfume :: Arbol -> Int
perfume = foldArbol (\c izq der -> izq + der + incSiIguales Flor c)
          (incSiIguales Flor)

--- 3
puedeVivir :: Arbol -> Bool
puedeVivir = foldArbol (\_ izq der -> izq || der) (== Hoja)

--- 4
follaje :: Arbol -> Int
follaje = foldArbol (\c izq der -> izq + der + incSiIguales Hoja c)
          (incSiIguales Hoja)

frutos :: Arbol -> Int
frutos = foldArbol (\c izq der -> izq + der + incSiIguales Fruto c)
         (incSiIguales Fruto)

mismosComponentes :: Arbol -> Arbol -> Bool
mismosComponentes a1 a2 = (peso a1 == peso a2) && (perfume a1 == perfume a2) &&
                          (follaje a1 == follaje a2) && (frutos a1 == frutos a2)

--Ejercicio 3

masPesado :: [Arbol] -> Arbol
masPesado = foldl1 (\best a -> if peso a > peso best then a else best)

--Ejercicio 4

crecer :: (Componente -> Componente) -> Arbol -> Arbol
crecer f = foldArbol (\c izq der -> Rama (f c) izq der)
            (Brote . f)

hojaAFlor :: Componente -> Componente
hojaAFlor Hoja = Flor
hojaAFlor x    = x

ultimaPrimavera :: Arbol -> Arbol
ultimaPrimavera = crecer hojaAFlor

-- Ejercicio 5

-- La razón por la cual este método no puede realizarse con foldArbol, es que
-- se necesita determinar en que nivel del árbol se encuentra el método
-- comer para tomar una u otra acción. Con el método foldArbol, siempre se
-- ejecutaria el codigo para todo elemento que matchee el caso base correspondiente
-- , con lo cual no se tiene ese tipo de granularidad necesaria. En particular,
-- en este método se necesita conocer cuántos niveles se recorrió del árbol para
-- saber desde que elemento el animal afectará el árbol a la hora de comer.

morder :: TipoHambre -> Arbol -> Arbol
morder Gula x      = x
morder Hambre x    = if perfume x > 0 then x else Brote Madera
morder Inanicion _ = Brote Madera

comer :: Animal -> Arbol -> Arbol
comer (_, 0, tipo) (Rama Fruto a1 a2) = morder tipo (Rama Fruto a1 a2)
comer (_, 0, tipo) (Brote Fruto) = morder tipo (Brote Fruto)
comer (_, 0, _) _ = error "Animal comiendo algo que no es un fruto"
comer _ (Brote _) = error "Altura invalida"
-- Los dos siguientes casos sirven para que, si al recorrer el arbol yendo
-- por una direccion dada, haya llegado a un brote pero aún falte recorrer para
-- llegar al nivel deseado.
comer (Derecha, alt, tipo) (Rama c a1 (Brote x)) | alt > 1 = Rama c (comer (Derecha, alt - 1, tipo) a1) (Brote x)
comer (Izquierda, alt, tipo) (Rama c (Brote x) a2) | alt > 1 = Rama c (Brote x) (comer (Izquierda, alt - 1, tipo) a2)
comer (dir, alt, tipo) (Rama c a1 a2) = case dir of Izquierda -> Rama c (rec a1) a2
                                                    Derecha -> Rama c a1 (rec a2)
                                        where rec = comer (dir, alt - 1, tipo)

-- Ejercicio 6
alimentar :: Arbol -> [Animal] -> Arbol
alimentar = foldl (flip comer)

-- Ejercicio 7
sobrevivientes :: [Animal] -> [Arbol] -> [Arbol]
-- sobrevivientes animales = filter (\x -> puedeVivir(alimentar x animales))
sobrevivientes animales = filter $ puedeVivir.(flip alimentar animales)

---

foldArbol :: (Componente -> b -> b -> b) -> (Componente -> b) -> Arbol -> b
foldArbol fRama fBrote t = case t of Rama c t1 t2 -> fRama c (rec t1) (rec t2)
                                     Brote c      -> fBrote c
                                     where rec = foldArbol fRama fBrote

-- Ejercicio 8

componentesPorNivel :: Arbol -> Int -> Int
componentesPorNivel a nivel = foldArbol (\_ izq der -> (const 1)) (const 1) a
--componentesPorNivel a nivel = foldArbol (\_ izq der -> \m -> if altura m == nivel then 1 else izq (m + 1) + der (m + 1)) (const 1) a

altura :: Arbol -> Int
altura = foldArbol (\_ izq der -> 1 + max izq der) (const 1)

ancho :: Arbol -> Int
ancho a = maximum [ componentesPorNivel a i | i <- [1..altura a]]

dimensiones :: Arbol -> (Int, Int)
dimensiones a = (altura a, ancho a)

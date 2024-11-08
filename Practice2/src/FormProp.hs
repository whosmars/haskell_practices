
module FormulasProposicionales where

import Operadores
import Data.List

-- Representa variables p, q, r, s...
type Atom = String

-- Representa las variables que se evalúan a True.
type State = [Atom]

-- data Prop
data Prop = Var Atom
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Impl Prop Prop
          | Syss Prop Prop

-- 1. Crear una instancia de la clase Show para Prop.
-- Nos ayudamos del tipo de dato Atom, que a su vez es un sinónimo de String.
instance Show Prop where
  show (Var p) = show p
  show (Neg p) = "¬" ++ show p
  show (Conj p q) = show p ++ " ^ " ++ show q
  show (Disy p q) = show p ++ " v " ++ show q
  show (Impl p q) = show p ++ " -> " ++ show q
  show (Syss p q) = show p ++ " <-> " ++ show q

-- 2. Crear una instancia de la clase Eq para Prop.
-- Casos atómicos.
instance Operadores Prop where
  (¬) = Neg
  (/\) = Conj
  (\/) = Disy
  (-->) = Impl
  (<-->) = Syss

---------------------------------------------------------------------------------
--------                             FUNCIONES                           --------
---------------------------------------------------------------------------------

-- Funcion que dada una formula, regresa el conjunto de todos los
-- símbolos que aparecen en ella.
vars :: Prop -> [Atom]
vars (Var p) = [p]
vars (Neg p) = vars p
vars (Conj p q) = nub (vars p ++ vars q)
vars (Disy p q) = nub (vars p ++ vars q)
vars (Impl p q) = nub (vars p ++ vars q)
vars (Syss p q) = nub (vars p ++ vars q)

-- Funcion que evalua una proposicion dado un estado.
interp :: State -> Prop -> Bool
interp s (Var x) = x `elem` s
interp s (Neg p) = not (interp s p)
interp s (Conj p q) = interp s p && interp s q
interp s (Disy p q) = interp s p || interp s q
interp s (Impl p q) = not (interp s p) || interp s q
interp s (Syss p q) = interp s p == interp s q

{-
State = ["p"]
Prop  = Conj (Var "p") (Var "q")
-}

-- Funcion que elimina las equivalencias (<->).
elimEquiv :: Prop -> Prop
elimEquiv (Var x) = Var x
elimEquiv (Neg p) = Neg (elimEquiv p)
elimEquiv (Conj p q) = Conj (elimEquiv p) (elimEquiv q)
elimEquiv (Disy p q) = Disy (elimEquiv p) (elimEquiv q)
elimEquiv (Impl p q) = Impl (elimEquiv p) (elimEquiv q)
elimEquiv (Syss p q) = Conj (Impl (elimEquiv p) (elimEquiv q)) (Impl (elimEquiv q) (elimEquiv p)) --aqui se elimina la equivalencia


-- Funcion que elimina las implicaciones, puedes suponer que no hay
-- equivalencias.
elimImpl :: Prop -> Prop
elimImpl (Var x) = Var x
elimImpl (Neg p) = Neg (elimImpl p)
elimImpl (Conj p q) = Conj (elimImpl p) (elimImpl q)
elimImpl (Disy p q) = Disy (elimImpl p) (elimImpl q)
elimImpl (Impl p q) = Disy (Neg (elimImpl p)) (elimImpl q)
elimImpl p = p  -- Esto manejará cualquier otro caso, si existen más constructores.

{-
P -> (Q -> R) => ¬P v (Q -> R) => ¬P v (¬Q v R)
-}

-- Funcion que da TODAS las posibles interpretaciones que podria tomar
-- una formula.
posiblesInterp :: Prop -> [State]
-- El $ omite los parentesis. Se crean todas las posibles interpretaciones con potencia
posiblesInterp formula = potencia $ vars formula

-- Funicion que nos dice si un estado es modelo de una proposicion.
esModelo :: Prop -> State -> Bool
-- El orden de los argumentos es inverso al de la funcion interp.
esModelo = flip interp

-- Funcion que nos da TODOS los modelos de una proposicion.
todosModelos :: Prop -> [State]
--Filtra los modelos de una proposicion.
todosModelos p = filter (esModelo p) (posiblesInterp p)

-- Funcion que nos dice si una proposicion es satifacible.
esSatisfacible :: Prop -> Bool
esSatisfacible p = not . null $ todosModelos p

-- Funcion que nos dice si una proposicion es instisfacible.
esInsatisfacible :: Prop -> Bool
esInsatisfacible = not . esSatisfacible   

-- Funcion que nos dice si una proposicion es una tautologia.
esTautologia :: Prop -> Bool
-- Verifica que todas las posibles interpretaciones sean modelos de la proposicion.
esTautologia p = all (`interp` p) (posiblesInterp p)

-- Funcion que nos dice si una proposicion es una contradiccion.
esContradiccion :: Prop -> Bool
-- Verifica que ninguna de las posibles interpretaciones sean modelos de la proposicion.
esContradiccion p = all (not . (`interp` p)) (posiblesInterp p)

---------------------------------------------------------------------------------
--------                           AUXILIARES                            --------
---------------------------------------------------------------------------------

potencia :: [a] -> [[a]]
potencia [] = [[]]
potencia (x:xs) = [ x:ys | ys <- xss] ++ xss
  where
    xss = potencia xs

---------------------------------------------------------------------------------
--------                             EJEMPLOS                            --------
---------------------------------------------------------------------------------

i  = Conj (Var "p") (Var "q")
i' = (Var "p") /\ (Var "q")

p = Var "p"
q = Var "q"
r = Var "r"
varsrp = ["r", "p"]
form1 = ((p \/ q) /\ (((¬) q) \/ r))
interp1 = interp varsrp form1

taut1 = (p \/ (¬) p)
taut2 = ((p \/ q) \/ ((¬)p /\ (¬) q))

cont1 = ((p \/ q) /\ ((¬)p /\ (¬) q))

potencia1 = potencia [1,2,3]

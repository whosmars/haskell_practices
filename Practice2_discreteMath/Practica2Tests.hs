{-# LANGUAGE TemplateHaskell #-}
module Practica2Tests where

import Practica2
import Test.QuickCheck


prop_productoPunto3D_conmutativa :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
prop_productoPunto3D_conmutativa u v = productoPunto3D u v == productoPunto3D v u

prop_productoPunto :: [Int] -> Property
prop_productoPunto v = not (null v) && any (/= 0) v ==> productoPunto v v > 0

prop_productoPunto_conmutativa :: Property
prop_productoPunto_conmutativa = forAll (listOf $ choose (0, 100)) (\xs -> forAll (vectorOf (length xs) $ choose (0, 100)) (\ys -> length xs == length ys ==> productoPunto xs ys == productoPunto ys xs))

prop_productoCruz_cancelacion :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
prop_productoCruz_cancelacion u v = productoPunto3D u (productoCruz u v) == 0

prop_restaVectores :: (Int, Int, Int) -> Bool
prop_restaVectores u = sumaVectores u (restaVectores (0, 0, 0) u) == (0, 0, 0)
  where
    sumaVectores :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
    sumaVectores (x1, x2, x3) (y1, y2, y3) = (x1 + y1, x2 + y2, x3 + y3)

prop_restaVectores_ceros :: (Int, Int, Int) -> Bool
prop_restaVectores_ceros u = restaVectores u u == (0, 0, 0)

prop_normalATriangulo :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> Property
prop_normalATriangulo a b c = v /= (0, 0, 0) && w /= (0, 0, 0) ==> productoPunto3D n v == 0 && productoPunto3D n w == 0
  where
    n@(nx, ny, nz) = normalATriangulo a b c
    v@(vx, vy, vz) = restaVectores b a
    w@(wx, wy, wz) = restaVectores c a


prop_divisoresPropios :: Positive Int -> Property
prop_divisoresPropios (Positive n) = n >= 2 ==> all (\p -> p > 0 && p < n && mod n p == 0) ps && length ps <= n - 1
  where
    ps = divisoresPropios n

prop_sumaPares :: Positive Int -> Property
prop_sumaPares (Positive n) = n > 1 ==> sumaPares n == n * (n + 1)

prop_esPrimo :: Positive Int -> Property
prop_esPrimo (Positive n) = n > 0 ==> null [x | x <- [2 .. (m - 1)], m `mod` x == 0]
  where
    m = primo n

prop_sumaDigitos :: NonNegative Int -> Property
prop_sumaDigitos (NonNegative n) = n >= 0 ==> s >= 0 && (s `mod` 9 == n `mod` 9)
  where
    s = sumaDigitos n

prop_imprimeUnEntero :: Int -> Bool
prop_imprimeUnEntero n = imprimeUnEntero n == show n

-- Ignoren el return [], solo es un problema del alcance con Haskell
return []
runTests = $quickCheckAll
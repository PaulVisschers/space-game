{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, TypeFamilies #-}
module Data.LinearAlgebra (
  module Data.Algebra,
  module Data.Vector,
  dot, length, normalize, dual, cross,
  translate, rotate, rotateByAngle, scale, uscale,
  ) where

import Prelude (Eq, Int, (==), fmap)


import Data.Algebra
import Data.Vector

instance (Group a, IsNat n) => Group (Vector n a) where
  zero = repeat zero
  negate = fmap negate
  (+) = zipWith (+)
  (-) = zipWith (-)

instance Ring a => Multiplicative a (Vector n a) where
  type Multiplied a (Vector n a) = Vector n a
  x * v = fmap (x *) v

instance Ring a => Multiplicative (Matrix n m a) (Vector m a) where
  type Multiplied (Matrix n m a) (Vector m a) = Vector n a
  m * v = fmap (dot v) m

instance (IsNat o, Ring a) => Multiplicative (Matrix n m a) (Matrix m o a) where
  type Multiplied (Matrix n m a) (Matrix m o a) = Vector n (Vector o a)
  m1 * m2 = fmap (transpose m2 *) m1

instance (IsNat n, Ring a) => Ring (Matrix n n a) where
  one = unfoldr f 0 where
    f :: (IsNat n, Ring a) => Int -> (Vector n a, Int)
    f n = (unfoldr (\(n, x) -> if n == x then (one, (n, x + (one :: Int))) else (zero, (n, x + (one :: Int)))) (n, zero), n + (one :: Int))

dot :: Ring a => Vector n a -> Vector n a -> a
dot x y = sum (zipWith (*) x y)

translate :: (Group a, IsNat n) => Vector n a -> Vector n a -> Vector n a
translate = (+)

rotate :: (IsNat n, Ring a) => Matrix n n a -> Vector n a -> Vector n a
rotate vs = (transpose vs *)

rotateByAngle :: Real a => a -> Vector3 a -> Vector3 a -> Vector3 a
rotateByAngle a (Cons x (Cons y (Cons z Nil))) = rotate (vector3
  (vector3 (x * x * mc + c) (y * x * mc + z * s) (z * x * mc - y * s))
  (vector3 (x * y * mc - z * s) (y * y * mc + c) (z * y * mc + x * s))
  (vector3 (x * z * mc + y * s) (y * z * mc - x * s) (z * z * mc + c))) where
    c = cos a
    mc = one - c
    s = sin a

scale :: Ring a => Vector n a -> Vector n a -> Vector n a
scale = zipWith (*)

uscale :: Ring a => a -> Vector n a -> Vector n a
uscale = (*)

length :: Real a => Vector n a -> a
length v = sqrt (dot v v)

normalize :: (Eq a, IsNat n, Real a) => Vector n a -> Vector n a
normalize v = if l == zero then zero else fmap (/ l) v where
  l = length v

dual :: Group a => Vector3 a -> Vector3 (Vector3 a)
dual (Cons x (Cons y (Cons z Nil))) = vector3 x' y' z' where
  x' = vector3 zero (negate z) y
  y' = vector3 z zero (negate x)
  z' = vector3 (negate y) x zero

cross :: Ring a => Vector3 a -> Vector3 a -> Vector3 a
cross a b = dual a * b

--radiansToDegrees :: Double -> Double
--radiansToDegrees x = x / pi * 180
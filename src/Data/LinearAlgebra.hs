{-# LANGUAGE GADTs #-}
module Data.LinearAlgebra where

import Prelude hiding (Num (..), (/), zipWith)

import Data.Algebra
import Data.Vector

degrees :: Double -> Double
degrees x = x / pi * 180

translate :: (Group a, Nat n) => Vector n a -> Vector n a -> Vector n a
translate = (+)

rotate :: (Nat n, Ring a) => Matrix n n a -> Vector n a -> Vector n a
rotate vs = (transpose vs *>)

rotateByAngle :: (Floating a, Ring a) => a -> Vector3 a -> Vector3 a -> Vector3 a
rotateByAngle a (Cons x (Cons y (Cons z Nil))) = rotate (vector3
  (vector3 (x * x * mc + c) (y * x * mc + z * s) (z * x * mc - y * s))
  (vector3 (x * y * mc - z * s) (y * y * mc + c) (z * y * mc + x * s))
  (vector3 (x * z * mc + y * s) (y * z * mc - x * s) (z * z * mc + c))) where
    c = cos a
    mc = 1 - c
    s = sin a

scale :: (Nat n, Ring a) => Vector n a -> Vector n a -> Vector n a
scale = zipWith (*)

uscale :: (Nat n, Ring a) => a -> Vector n a -> Vector n a
uscale = (*>)
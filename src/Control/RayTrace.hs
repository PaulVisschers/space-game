{-# LANGUAGE GADTs #-}
module Control.RayTrace where

import Prelude hiding (negate, (-), (/), minimum, maximum, any, all)
import Data.Foldable

import Data.Algebra
import Data.Vector

data RayTraceResult f a = Miss | Inside | Hit (a, f a) (a, f a) deriving (Eq, Ord, Show)

rayTraceAABB :: (DivisionRing a, Nat n, Ord a) => Vector n a -> Vector n a -> Vector2 (Vector n a) -> RayTraceResult (Vector n) a
rayTraceAABB pos dir (Cons low (Cons high Nil))
  | any (== Miss) dims = Miss
  | all (== Inside) dims = Inside
  | fst near > fst far = Miss
  | fst far < zero = Miss
  | otherwise = Hit near far where
    dims = zipWith5 rayTraceAABB1D pos dir low high one
    (nears, fars) = Prelude.unzip . hits . toList $ dims
    near = maximum nears
    far = minimum fars

rayTraceAABB1D :: (DivisionRing a, Ord a) => a -> a -> a -> a -> Vector n a -> RayTraceResult (Vector n) a
rayTraceAABB1D pos dir low high normal 
  | dir == zero = if pos < low || pos > high then Miss else Inside
  | otherwise = Hit near far where
    distX = (low - pos) / dir
    distY = (high - pos) / dir
    x = (distX, fmap negate normal)
    y = (distY, normal)
    near = if distX <= distY then x else y
    far = if distX > distY then x else y

hits :: [RayTraceResult f a] -> [((a, f a), (a, f a))]
hits [] = []
hits (Hit x y:xs) = (x, y) : hits xs
hits (_:xs) = hits xs

isHit :: RayTraceResult f a -> Bool
isHit (Hit _ _) = True
isHit _ = False
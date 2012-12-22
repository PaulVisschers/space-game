{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TemplateHaskell, TypeOperators #-}
module Common where

import Prelude hiding ((+), (-), (*), (/), negate, zipWith, repeat, any, all, minimum, maximum)
import Data.Foldable as Foldable

import Data.Map as Map
import Data.Set as Set
import Data.Label
import qualified Data.Time.Clock as Clock

import Data.Vector
import Data.Algebra
import Data.Body

newtype Key a = Key Int deriving (Eq, Ord, Enum, Show, Read)

type DataStore a = Map.Map (Key a) a

key :: Key a -> Map.Map (Key a) b :-> b
key ix = lens (Map.! ix) (Map.insert ix)

data ClientMessage = Connect | KeyDown WalkingKey | KeyUp WalkingKey | MouseLook (Vector2 Int) deriving (Show, Read)
data ServerMessage = ConnectSuccess (Key Player) Scene | FullUpdate Scene deriving (Show, Read)

data WalkingKey = WalkForward | WalkBackward | WalkRight | WalkLeft | WalkUp | WalkDown deriving (Eq, Ord, Show, Read)

-- * Data
data Player = Player {
  _playerBody :: Body
  } deriving (Show, Read)

data Scene = Scene {
  _players :: DataStore Player
  } deriving (Show, Read)

newScene = Scene Map.empty

newPlayer = Player (Body 0 startPos zeroComps zeroComps) where
  zeroComps = Components zero zero
  startPos = Components (vector3 0 0 (-2)) one

$(mkLabels [''Player, ''Scene])

instance IsBody Player where
  body = playerBody

timeDiff :: Clock.UTCTime -> Clock.UTCTime -> Double
timeDiff x y = fromRational $ toRational $ Clock.diffUTCTime x y

-- Ray tracing
data RayTraceResult f a = Miss | Inside | Hit (a, f a) (a, f a) deriving (Eq, Ord, Show)

rayTraceAABB :: (DivisionRing a, Nat n, Ord a) => Vector n a -> Vector n a -> Vector2 (Vector n a) -> RayTraceResult (Vector n) a
rayTraceAABB pos dir (Cons low (Cons high Nil))
  | any (== Miss) dims = Miss
  | all (== Inside) dims = Inside
  | fst near > fst far = Miss
  | fst far < zero = Miss
  | otherwise = Hit near far where
    dims = zipWith5 rayTraceAABB1D pos dir low high one
    (nears, fars) = Prelude.unzip . hits . Foldable.toList $ dims
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
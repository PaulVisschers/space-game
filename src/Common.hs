{-# LANGUAGE DeriveGeneric, GADTs, GeneralizedNewtypeDeriving, TemplateHaskell, TypeOperators #-}
module Common where

import Prelude hiding (id, (.), (+), (-), (*), (/), negate, zipWith, repeat, any, all, minimum, maximum, sum, pi)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Label
import Data.Label.PureM
import Control.Category
import Control.Monad.State.Class (MonadState)
import Data.Time.Clock (UTCTime, diffUTCTime)
import GHC.Generics (Generic)
import Control.Applicative ((<$>))

import Data.Vector
import Data.Algebra
import Data.Body
import Data.BlockObject
import Data.LinearAlgebra

import Data.Binary (Binary)

newtype Ref a = Ref Int deriving (Eq, Ord, Enum, Show, Read)

item :: Ref a -> Map (Ref a) b :-> b
item ix = lens (M.! ix) (M.insert ix)

data ClientMessage = Connect | Ping Id InputChange deriving (Show, Read, Generic)
data ServerMessage = ConnectSuccess (Ref Player) Scene | FullUpdate Scene deriving (Show, Read)

type Id = Int
type TimeDelta = Double
data InputChange = NoChange | KeyChange KeyState WalkingKey | MouseChange (Vector2 Int) deriving (Show, Read, Generic)
data KeyState = KeyUp | KeyDown deriving (Show, Read, Generic)
data WalkingKey = WalkForward | WalkBackward | WalkRight | WalkLeft | WalkUp | WalkDown deriving (Eq, Ord, Show, Read, Generic)

instance Binary ClientMessage
instance Binary InputChange
instance Binary KeyState
instance Binary WalkingKey

-- * Data
data Player = Player {
  _playerBody :: Body,
  _pressedKeys :: Set WalkingKey,
  _movementSpeed :: Double, -- in m/s.
  _rotationSpeedX :: Double, -- in radians/pixel.
  _rotationSpeedY :: Double -- in radians/pixel.
  } deriving (Show, Read)

data Scene = Scene {
  _players :: Map (Ref Player) Player,
  _playersKeys :: Map (Ref Player) (Set WalkingKey),
  _blockObjects :: Map (Ref BlockObject) BlockObject
  } deriving (Show, Read)

newScene :: Scene
newScene = Scene M.empty M.empty (M.singleton (Ref 0) testBlockObject)

newPlayer :: Player
newPlayer = Player (Body 0 startPos zeroComps zeroComps) Set.empty 1.4 (pi / 2000) (pi / 2000) where
  zeroComps = Components zero zero
  startPos = Components (vector3 0 0 (-2)) one

$(mkLabels [''Player, ''Scene])

player :: GameState st => Ref Player -> st :-> Player
player ref = item ref . players . scene

playerKeys :: GameState st => Ref Player -> st :-> Set WalkingKey
playerKeys ref = item ref . playersKeys . scene

instance IsBody Player where
  body = playerBody

diffTime :: UTCTime -> UTCTime -> TimeDelta
diffTime x y = fromRational $ toRational $ diffUTCTime x y -- result is x - y

walkRotation :: Vector3 (Vector3 Double) -> Vector3 (Vector3 Double)
walkRotation dir = vector3 right up front where
  up = vector3 0 1 0
  right = vx dir
  front = cross right up

velocityFromKey :: WalkingKey -> Vector3 Double
velocityFromKey WalkLeft = vector3 1 0 0
velocityFromKey WalkRight = vector3 (-1) 0 0
velocityFromKey WalkDown = vector3 0 (-1) 0
velocityFromKey WalkUp = vector3 0 1 0
velocityFromKey WalkBackward = vector3 0 0 (-1)
velocityFromKey WalkForward = vector3 0 0 1

velocityFromKeys :: Set WalkingKey -> Vector3 Double
velocityFromKeys = Set.foldr (\x y -> velocityFromKey x + y) zero

setPlayerVelocity :: Set WalkingKey -> Player -> Player
setPlayerVelocity keys player = set linVel vel player where
  dir = get angPos player
  speed = get movementSpeed player
  vel = rotate (walkRotation dir) (speed * normalize (velocityFromKeys keys))

turnPlayer :: Vector2 Int -> Player -> Player
turnPlayer mouseMovement player = set angPos rotatedXAndY player where
  rotateX = rotateByAngle (fromIntegral (vx mouseMovement) *. negate speedX) (vector3 0 1 0)
  rotateY = rotateByAngle (fromIntegral (vy mouseMovement) *. speedY) (vx direction)
  speedX = get rotationSpeedX player
  speedY = get rotationSpeedY player
  direction = get angPos player
  rotatedY = rotateY <$> direction
  rotatedXAndY = if vy (vy rotatedY) >= 0 then rotateX <$> rotatedY else rotateX <$> direction

class GameState a where
  scene :: a :-> Scene

instance GameState Scene where
  scene = id
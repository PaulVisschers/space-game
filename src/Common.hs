{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TemplateHaskell, TypeOperators #-}
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

import Data.Vector
import Data.Algebra
import Data.Body
import Data.BlockObject
import Data.LinearAlgebra

newtype Ref a = Ref Int deriving (Eq, Ord, Enum, Show, Read)

item :: Ref a -> Map (Ref a) b :-> b
item ix = lens (M.! ix) (M.insert ix)

data ClientMessage = Connect | Ping Id InputChange deriving (Show, Read)
data ServerMessage = ConnectSuccess (Ref Player) Scene | FullUpdate Scene deriving (Show, Read)

type Id = Int
type TimeDelta = Double
data InputChange = NoChange | KeyChange KeyState WalkingKey | MouseChange (Vector2 Int) deriving (Show, Read)
data KeyState = KeyUp | KeyDown deriving (Show, Read)
data WalkingKey = WalkForward | WalkBackward | WalkRight | WalkLeft | WalkUp | WalkDown deriving (Eq, Ord, Show, Read)

-- * Data
data Player = Player {
  _playerBody :: Body,
  _pressedKeys :: Set WalkingKey,
  _movementSpeed :: Double, -- in m/s.
  _rotationSpeed :: Double -- in radians/pixel.
  } deriving (Show, Read)

data Scene = Scene {
  _players :: Map (Ref Player) Player,
  _playersKeys :: Map (Ref Player) (Set WalkingKey),
  _blockObjects :: Map (Ref BlockObject) BlockObject
  } deriving (Show, Read)

newScene :: Scene
newScene = Scene M.empty M.empty (M.singleton (Ref 0) testBlockObject)

newPlayer :: Player
newPlayer = Player (Body 0 startPos zeroComps zeroComps) Set.empty 1.4 (pi / 500) where
  zeroComps = Components zero zero
  startPos = Components (vector3 0 0 (-2)) one

--type TickId = Int
--type Offset = Double -- In seconds
--type Duration = Double -- In seconds

--data UserInput = UserInput {
--  _tickId :: Int,
--  _tickDuration :: Duration,
--  _mouseMovement :: Vector2 Int,
--  _keyChanges :: [KeyChange]
--} deriving (Show, Read)

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

playerOrientation :: Vector2 Int -> Vector3 (Vector3 Double) -> Vector3 (Vector3 Double)
playerOrientation mouse dir = if vy (vy rotatedY) >= 0 then fmap rotX rotatedY else fmap rotX dir where
  rotX = rotateByAngle (fromIntegral (vx mouse) *. (-0.001)) (vector3 0 1 0)
  rotY = rotateByAngle (fromIntegral (vy mouse) *. 0.001) (vx dir)
  rotatedY = fmap rotY dir

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

class GameState a where
  scene :: a :-> Scene

instance GameState Scene where
  scene = id

--processKeyChange :: (GameState st, MonadState st m) => Ref Player -> KeyChange -> m Duration
--processKeyChange ref (KeyChange dur state key) = do
--  let keys = pressedKeys . player ref
--  keys =. case state of
--    KeyDown -> Set.insert key
--    KeyUp -> Set.delete key
--  newKeys <- gets keys
--  player ref =. setPlayerVelocity newKeys . updateBody dur
--  return dur

--processKeyChanges :: (GameState st, MonadState st m) => Ref Player -> Duration -> [KeyChange] -> m ()
--processKeyChanges ref dur chs = do
--  durs <- mapM (processKeyChange ref) chs
--  let dur2 = sum durs
--  if dur > dur2
--    then do
--      player ref =. updateBody (dur - dur2)
--      return ()
--    else return ()

--processUserInput :: (GameState st, MonadState st m) => Ref Player -> UserInput -> m ()
--processUserInput ref input = do
--  let dur = get tickDuration input
--  let chs = get keyChanges input
--  processKeyChanges ref dur chs
--  -- TODO: Process mouse input

--data UserInput = UserInput {
--  _tickId :: Int,
--  _tickDuration :: Duration,
--  _mouseMovement :: Vector2 Int,
--  _keyChanges :: [KeyChange]
--} deriving (Show, Read)
--data KeyChange = KeyChange Offset KeyState WalkingKey deriving (Show, Read)
--data KeyState = KeyUp | KeyDown deriving (Show, Read)

--test1 = setPlayerVelocity (Set.singleton WalkForward) newPlayer
--test2 = updateBody 1 test1
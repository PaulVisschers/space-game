{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TemplateHaskell, TypeOperators #-}
module Common where

import Prelude hiding ((+), (-), (*), (/), negate, zipWith, repeat, any, all, minimum, maximum)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Label
import Data.Time.Clock (UTCTime, diffUTCTime)

import Data.Vector
import Data.Algebra
import Data.Body
import Data.BlockObject

newtype Ref a = Ref Int deriving (Eq, Ord, Enum, Show, Read)

item :: Ref a -> Map (Ref a) b :-> b
item ix = lens (M.! ix) (M.insert ix)

data ClientMessage = Connect | InputChanges UserInput deriving (Show, Read)
data ServerMessage = ConnectSuccess (Ref Player) Scene | FullUpdate Scene deriving (Show, Read)

data WalkingKey = WalkForward | WalkBackward | WalkRight | WalkLeft | WalkUp | WalkDown deriving (Eq, Ord, Show, Read)

-- * Data
data Player = Player {
  _playerBody :: Body
  } deriving (Show, Read)

data Scene = Scene {
  _players :: Map (Ref Player) Player,
  _blockObjects :: Map (Ref BlockObject) BlockObject
  } deriving (Show, Read)

newScene :: Scene
newScene = Scene M.empty (M.singleton (Ref 0) testBlockObject)

newPlayer :: Player
newPlayer = Player (Body 0 startPos zeroComps zeroComps) where
  zeroComps = Components zero zero
  startPos = Components (vector3 0 0 (-2)) one

--data TickUserInput = TickUserInput {
--  _tickStartTime :: UTCTime,
--  _tickDuration :: Duration,
--  _keyChanges :: [KeyChange],
--  _mouseMovements :: [MouseMovement]
--  _inputChanges :: [(Offset, InputChange)]
--  } deriving (Show, Read)

type TickId = Int
type Offset = Double -- In microseconds (TODO: Verify that this is in microseconds)
type Duration = Double -- In microseconds (TODO: Verify that this is in microseconds)

data UserInput = UserInput {
  _tickId :: Int,
  _tickDuration :: Duration,
  _mouseMovement :: Vector2 Int,
  _keyChanges :: [KeyChange]
} deriving (Show, Read)

data KeyChange = KeyChange Offset KeyState WalkingKey deriving (Show, Read)
data KeyState = KeyUp | KeyDown deriving (Show, Read)

$(mkLabels [''Player, ''Scene, ''UserInput])


instance IsBody Player where
  body = playerBody

diffTime :: UTCTime -> UTCTime -> Duration
diffTime x y = fromRational $ toRational $ diffUTCTime x y -- result is x - y
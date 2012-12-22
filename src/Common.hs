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
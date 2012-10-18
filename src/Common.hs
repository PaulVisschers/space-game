{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, TypeOperators #-}
module Common where

import qualified Data.Map as Map
import Data.Label
import qualified Data.Time.Clock as Clock

import Data.Vector

newtype Ref a = Ref Int deriving (Eq, Ord, Enum, Show, Read)

type Store a = Map.Map (Ref a) a

ref :: Ref a -> Store a :-> a
ref ix = lens (Map.! ix) (\x m -> Map.insert ix x m)

-- * Data
data Player = Player {
  _spatial :: Spatial
  } deriving (Show, Read)

data Spatial = Spatial {
  _position :: Combination,
  _velocity :: Combination,
  _acceleration :: Combination
  } deriving (Show, Read)
  
data Combination = Combination {
  _linear :: Vector3 Double,
  _angular :: Vector3 (Vector3 Double)
  } deriving (Show, Read)

data Scene = Scene {
  _players :: Store Player
  } deriving (Show, Read)

emptyScene = Scene Map.empty

$(mkLabels [''Player, ''Spatial, ''Combination, ''Scene])

data ClientMessage = Connect | KeyDown WalkingKey | KeyUp WalkingKey | MouseLook (Vector2 Int) deriving (Show, Read)
data ServerMessage = FullUpdate Scene deriving (Show, Read)

data WalkingKey = WalkForward | WalkBackward | WalkRight | WalkLeft | WalkUp | WalkDown deriving (Show, Read)

timeDiff :: Clock.UTCTime -> Clock.UTCTime -> Double
timeDiff x y = fromRational $ toRational $ Clock.diffUTCTime x y
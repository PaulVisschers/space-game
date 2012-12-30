{-# LANGUAGE TemplateHaskell #-}
module Data.Server where

import Data.Time.Clock (UTCTime)
import Data.Label
import Data.Map as Map (Map, empty)
import Data.Set as Set (Set, empty)

import Data.Algebra (zero)
import Data.Vector (Vector2)

import Common

data State = State {
  _inputs :: Map (Key Player) Input,
  _time :: Time,
  _scene :: Scene
  }

newState :: UTCTime -> State
newState now = State Map.empty (newTime now) newScene

data Input = Input {
  _keyState :: Set WalkingKey,
  _mouseLook :: Vector2 Int
  }

newInput :: Input
newInput = Input Set.empty zero

data Time = Time {
  _currentTick :: UTCTime,
  _previousTick :: UTCTime,
  _difference :: Double -- Seconds between this and previous tick.
}

newTime :: UTCTime -> Time
newTime now = Time now now 0

$(mkLabels [''State, ''Input, ''Time])
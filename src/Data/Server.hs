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
  _time :: Time,
  _serverScene :: Scene
  }

newState :: UTCTime -> State
newState now = State (newTime now) newScene

data Time = Time {
  _currentTick :: UTCTime,
  _previousTick :: UTCTime,
  _difference :: Double -- Seconds between this and previous tick.
}

newTime :: UTCTime -> Time
newTime now = Time now now 0

$(mkLabels [''State, ''Time])

instance GameState State where
  scene = serverScene
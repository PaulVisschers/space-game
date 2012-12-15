{-# LANGUAGE TemplateHaskell #-}
module Data.Server where

import Data.Time.Clock
import Data.Label

import Common

data State = State {
  _time :: Time,
  _scene :: Scene
  }

data Time = Time {
  _currentTick :: UTCTime,
  _previousTick :: UTCTime,
  _difference :: Double -- Seconds between this and previous tick.
}

$(mkLabels [''State, ''Time])
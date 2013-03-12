{-# LANGUAGE TemplateHaskell #-}
module Data.Client where

import Data.Time.Clock (UTCTime)
import Data.Label
import Data.Set (Set)

import Data.Vector (Vector2)
import Common

data State = State {
  _playerRef :: Maybe (Ref Player),
  --_nextTickId :: Int,
  --_currentTickStartTime :: UTCTime,
  --_currentTickMouseMovement :: Vector2 Int,
  --_currentTickKeyChanges :: [KeyChange],
  --_currentTickLastKeyPress :: UTCTime,
  --_currentTickPressedKeys :: Set WalkingKey,
  --_userInputs :: [UserInput],
  _clientScene :: Scene,
  _serverScene :: Scene,
  _serverSceneId :: Id
  }

$(mkLabels [''State])

instance GameState State where
  scene = clientScene
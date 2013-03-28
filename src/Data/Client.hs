{-# LANGUAGE TemplateHaskell #-}
module Data.Client where

import Data.Time.Clock (UTCTime)
import Data.Label
import Data.Set (Set)

import Data.Vector (Vector2)
import Common

data State = State {
  _playerRef :: Ref Player,
  _clientScene :: Scene,
  _sceneId :: Id
  }

$(mkLabels [''State])

instance GameState State where
  scene = clientScene
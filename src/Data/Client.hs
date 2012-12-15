{-# LANGUAGE TemplateHaskell #-}
module Data.Client where

import Data.Label

import Common

data State = State {
  _playerKey :: Maybe (Key Player),
  _scene :: Scene
  }

$(mkLabels [''State])
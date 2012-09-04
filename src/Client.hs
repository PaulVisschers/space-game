{-# LANGUAGE ScopedTypeVariables #-}
module Client where

import Control.Monad

import Data.Vector
import qualified Network.Channel.Client as Channel
import Common

main = do
  chan :: Channel.Channel ServerMessage ClientMessage <- Channel.connect "localhost" 3000
  forever $ do
    xs <- Channel.receive chan
    if null xs then return () else print xs
  
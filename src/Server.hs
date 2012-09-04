{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, TypeOperators #-}
module Server where

import Prelude hiding (id, (.))
import Control.Category
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad
import Data.Label
import qualified Data.Time.Clock as Clock
import Data.Function
import Control.Concurrent (threadDelay)

import Data.Vector
import qualified Network.Channel.Server.Trans as Channel
import Common

type GameMonad a = ReaderT (Channel.Channel (Ref Player) ClientMessage ServerMessage) IO a

main = do
  chan <- Channel.new 3000
  time <- Clock.getCurrentTime
  Channel.withChannel chan (tick (Scene Map.empty) time) 
  Channel.close chan

tick :: Scene -> Clock.UTCTime -> GameMonad ()
tick scene prevStepTime = do
  stepTime <- liftIO Clock.getCurrentTime
  let stepSize = timeDiff stepTime prevStepTime

  -- Get incoming messages and generate events

  -- Look at scene and generate events

  -- Update scene based on events and behaviour
  
  -- Update clients
  liftIO $ putStrLn "Updating players"
  Channel.broadcast (FullUpdate scene)
  
  time <- liftIO Clock.getCurrentTime
  liftIO $ threadDelay (round ((0.5 - timeDiff time stepTime) * 1000000))
  tick scene stepTime
{-# LANGUAGE TupleSections #-}
module Server where

import Prelude hiding (id, (.), (+), (-), (*), (/), negate, zipWith, repeat, any, all, minimum, maximum, foldl, recip)
import Data.Foldable hiding (mapM_, concatMap)
import Control.Category
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Monad.State (StateT, evalStateT)
import qualified Control.Monad.State.Class as State
import Control.Monad
import Data.Label
import Data.Label.PureM
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Function (on)
import Data.List (sortBy)
import Control.Concurrent (threadDelay)

import Data.Vector
import Data.Algebra
import Data.LinearAlgebra
import Data.BlockObject
import qualified Network.Channel.Server.Trans as Channel
import qualified Network.Channel.Server as Test
import Common
import Data.Body
import Data.Server

type ServerMonad a = StateT State (ReaderT (Channel.Channel (Ref Player) ClientMessage ServerMessage) IO) a

main = do
  chan <- Channel.new 3000
  now <- getCurrentTime
  let state = newState now
  Channel.withChannel chan (evalStateT (forever tick) state)
  Channel.close chan

tick :: ServerMonad ()
tick = do
  -- Updating time-based values to current tick
  now <- liftIO getCurrentTime
  before <- gets (currentTick . time)
  time =: Time now before (diffTime now before)

  -- Process all incoming messages
  processMessages

  -- Update state from previous tick to new one
  --ins <- gets inputs
  --players . scene =. updatePlayers ins
  --inputs =. Map.map (set mouseLook zero)

  -- Send new scene state to everyone
  sc <- gets scene
  Channel.broadcast (FullUpdate sc)

  now2 <- liftIO getCurrentTime
  let val = round ((recip 60 - diffTime now2 now) *. 1000000)
  liftIO (threadDelay val)

  return ()

processMessages :: ServerMonad ()
processMessages = do
  msgs <- messagesToList `fmap` Channel.receiveAll
  --liftIO (print msgs)
  mapM_ (uncurry processMessage) msgs
  return ()

processMessage :: Ref Player -> ClientMessage -> ServerMonad ()
processMessage ref msg = case msg of
  Connect -> do
    player ref =: newPlayer
    playerKeys ref =: Set.empty
    sc <- gets scene
    Channel.send ref (ConnectSuccess ref sc)
  Ping _ _ -> return () -- TODO: Actually process this.
--  KeyDown k -> keyState . item ref . inputs =. Map.insert k 
--  KeyUp k -> keyState . item ref . inputs =. Set.delete k
--  MouseLook v -> mouseLook . item ref . inputs =. (+ v)

messagesToList :: Map k [a] -> [(k, a)]
messagesToList = concatMap (\(k, xs) -> map (k,) xs) . Map.assocs

--updatePlayers :: Map (Ref Player) Input -> Map (Ref Player) Player -> Map (Ref Player) Player
--updatePlayers inputs = Map.mapWithKey (\k p -> updatePlayer (inputs Map.! k) p)

--updatePlayer :: Input -> Player -> Player
--updatePlayer input player = set position (Components lin ang) player where
--  lin = playerPosition (Map.keysSet $ get keyState input) (get angPos player) (get linPos player)
--  ang = playerOrientation (get mouseLook input) (get angPos player)
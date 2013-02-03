{-# LANGUAGE TupleSections #-}
module Server where

import Prelude hiding (id, (.), (+), (-), (*), (/), negate, zipWith, repeat, any, all, minimum, maximum, foldl)
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
  ins <- gets inputs
  players . scene =. updatePlayers ins
  inputs =. Map.map (set mouseLook zero)

  -- Send new scene state to everyone
  sc <- gets scene
  Channel.broadcast (FullUpdate sc)

  return ()

processMessages :: ServerMonad ()
processMessages = do
  msgs <- messagesToList `fmap` Channel.receiveAll
--  mapM_ (uncurry processMessage) msgs
  return ()

--processMessage :: Ref Player -> ClientMessage -> ServerMonad ()
--processMessage playerRef msg = case msg of
--  Connect -> do
--    item playerRef . players . scene =: newPlayer
--    item playerRef . inputs =: newInput
--    sc <- gets scene
--    Channel.send playerRef (ConnectSuccess playerRef sc)
--  KeyDown k -> keyState . item playerRef . inputs =. Map.insert k 
--  KeyUp k -> keyState . item playerRef . inputs =. Set.delete k
--  MouseLook v -> mouseLook . item playerRef . inputs =. (+ v)

messagesToList :: Map k [a] -> [(k, a)]
messagesToList = concatMap (\(k, xs) -> map (k,) xs) . Map.assocs

playerPosition :: Set WalkingKey -> Vector3 (Vector3 Double) -> Vector3 Double -> Vector3 Double
playerPosition keys dir pos = pos + lv where
  up = vector3 0 1 0
  right = vx dir
  front = cross right up
  lv = rotate (vector3 right up front) (fmap (* 0.002) $ normalize $ Set.foldr (\x y -> movementFromKey x + y) zero keys)

playerOrientation :: Vector2 Int -> Vector3 (Vector3 Double) -> Vector3 (Vector3 Double)
playerOrientation mouse dir = if vy (vy rotatedY) >= 0 then fmap rotX rotatedY else fmap rotX dir where
  rotX = rotateByAngle (fromIntegral (vx mouse) * (-0.001)) (vector3 0 1 0)
  rotY = rotateByAngle (fromIntegral (vy mouse) * 0.001) (vx dir)
  rotatedY = fmap rotY dir

movementFromKey :: WalkingKey -> Vector3 Double
movementFromKey WalkLeft = vector3 1 0 0
movementFromKey WalkRight = vector3 (-1) 0 0
movementFromKey WalkDown = vector3 0 (-1) 0
movementFromKey WalkUp = vector3 0 1 0
movementFromKey WalkBackward = vector3 0 0 (-1)
movementFromKey WalkForward = vector3 0 0 1

updatePlayers :: Map (Ref Player) Input -> Map (Ref Player) Player -> Map (Ref Player) Player
updatePlayers inputs = Map.mapWithKey (\k p -> updatePlayer (inputs Map.! k) p)

updatePlayer :: Input -> Player -> Player
updatePlayer input player = set position (Components lin ang) player where
  lin = playerPosition (Map.keysSet $ get keyState input) (get angPos player) (get linPos player)
  ang = playerOrientation (get mouseLook input) (get angPos player)
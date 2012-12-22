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
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Clock
import Data.Function (on)
import Data.List (sortBy)
import Control.Concurrent (threadDelay)

import Data.Vector
import Data.Algebra
import Data.LinearAlgebra
import qualified Network.Channel.Server.Trans as Channel
import qualified Network.Channel.Server as Test
import Common
import Data.Body
import Data.Server

type GameMonad a = StateT State (ReaderT (Channel.Channel (Key Player) ClientMessage ServerMessage) IO) a

main = do
  chan <- Channel.new 3000
  now <- Clock.getCurrentTime
  let state = newState now
  Channel.withChannel chan (evalStateT (forever tick) state)
  Channel.close chan

tick :: GameMonad ()
tick = do
  -- Updating time-based values to current tick
  now <- liftIO Clock.getCurrentTime
  before <- gets (currentTick . time)
  time =: Time now before (timeDiff now before)

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

  -- DEBUG
  -- msgs <- Channel.receiveAll
  -- liftIO $ if Map.null msgs then return () else print msgs

  -- Get incoming messages and generate events

  -- Look at scene and generate events

  -- Update scene based on events and behaviour
  
  -- Update clients
  --liftIO $ putStrLn "Updating players"
  -- Channel.broadcast (FullUpdate scene)
  
  -- time <- liftIO Clock.getCurrentTime
  -- liftIO $ threadDelay (round ((0.5 - timeDiff time stepTime) * 1000000))
  -- tick scene stepTime

  -- let input = state ! Input
  -- let scene = state ! Scene
  
  -- oldTime <- get (state ! CurrentTime)
  -- time <- getCurrentTime
  -- let dt = fromRational (toRational (diffUTCTime oldTime time))
  -- state ! CurrentTime $= time

  -- let player = scene ! Player
  -- pos <- get (player ! Position)

  -- -- Moves player by key presses
  -- keys <- get (input ! KeysPressed)
  -- dir <- get (player ! Orientation)
  -- (player ! Position) $~ playerPosition keys dir

  -- -- Rotates player by mouse movements
  -- mouse <- get (input ! MouseLook)
  -- (player ! Orientation) $~ playerOrientation mouse
  -- input ! MouseLook $= vector2 0 0

  -- -- Determines the coordinates of the block that is looked at, and where a new block is to be placed.
  -- pos <- get (player ! Position)
  -- dir <- get (player ! Orientation)
  -- grid <- get (scene ! Grid)
  -- let mplace = rayTraceBlocks 100 pos (vz dir) grid
  -- case mplace of
    -- Nothing -> do
      -- player ! ViewLocation $= Nothing
      -- player ! PlacementLocation $= snap (uscale 5 (pos + vz dir))
    -- Just (v, Hit (_, normal) _) -> do
      -- player ! ViewLocation $= Just v
      -- player ! PlacementLocation $= v + normal

  -- -- Places and picks blocks from grid
  -- let grid = scene ! Grid
  -- view <- get (player ! ViewLocation)
  -- placement <- get (player ! PlacementLocation)
  -- when (M.member PickBlock keys && isJust view && diffUTCTime time (keys M.! PickBlock) >= 0.25) $ do
    -- grid $~ setBlock (fromJust view) NoBlock
    -- (input ! KeysPressed) $~ M.insert PickBlock time
  -- when (M.member PlaceBlock keys && diffUTCTime time (keys M.! PlaceBlock) >= 0.25) $ do
    -- grid $~ setBlock placement TestBlock
    -- (input ! KeysPressed) $~ M.insert PlaceBlock time

  -- -- Exit game if requested
  -- when (M.member ExitGame keys) $ do
    -- GL.leaveMainLoop

  -- GL.postRedisplay Nothing

processMessages :: GameMonad ()
processMessages = do
  msgs <- messagesToList `fmap` Channel.receiveAll
  mapM_ (uncurry processMessage) msgs

processMessage :: Key Player -> ClientMessage -> GameMonad ()
processMessage playerKey msg = case msg of
  Connect -> do
    key playerKey . players . scene =: newPlayer
    key playerKey . inputs =: newInput
    sc <- gets scene
    Channel.send playerKey (ConnectSuccess playerKey sc)
  KeyDown k -> keyState . key playerKey . inputs =. Set.insert k
  KeyUp k -> keyState . key playerKey . inputs =. Set.delete k
  MouseLook v -> mouseLook . key playerKey . inputs =. (+ v)

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

updatePlayers :: Map (Key Player) Input -> DataStore Player -> DataStore Player
updatePlayers inputs = Map.mapWithKey (\k p -> updatePlayer (inputs Map.! k) p)

updatePlayer :: Input -> Player -> Player
updatePlayer input player = set position (Components lin ang) player where
  lin = playerPosition (get keyState input) (get angPos player) (get linPos player)
  ang = playerOrientation (get mouseLook input) (get angPos player)

-- rayTraceBlocks :: Double -> Vector3 Double -> Vector3 Double -> Map (Vector3 Double) Block -> Maybe (Vector3 Double, RayTraceResult Vector3 Double)
-- rayTraceBlocks maxDist pos dir grid = if null sorted || nearDist > maxDist then Nothing else Just (P.head sorted) where
  -- aabbs = M.mapWithKey (\k x -> Cons k (Cons (k + repeat one) Nil)) grid
  -- rays = M.map (rayTraceAABB (uscale 5 pos) dir) aabbs
  -- hits = M.toList (M.filter isHit rays)
  -- sorted = sortBy (P.compare `on` \(_, Hit (dist, _) _) -> dist) hits
  -- (_, Hit (nearDist, _) _) = P.head sorted
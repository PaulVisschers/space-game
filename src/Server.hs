{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TemplateHaskell, TypeOperators #-}
module Server where

import Prelude hiding (id, (.), (+), (-), (*), (/), negate, zipWith, repeat, any, all, minimum, maximum)
import Data.Foldable
import Control.Category
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad
import Data.Label
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Clock
import Data.Function (on)
import Data.List (sortBy)
import Control.Concurrent (threadDelay)

import Data.Vector
import Data.Algebra
import qualified Network.Channel.Server.Trans as Channel
import Common

type GameMonad a = ReaderT (Channel.Channel (Ref Player) ClientMessage ServerMessage) IO a

main = do
  chan <- Channel.new 3000
  time <- Clock.getCurrentTime
  Channel.withChannel chan (tick (Scene Map.empty) time) 
  Channel.close chan

tick :: Scene -> UTCTime -> GameMonad ()
tick scene prevStepTime = do
  stepTime <- liftIO Clock.getCurrentTime
  let stepSize = timeDiff stepTime prevStepTime

  -- DEBUG
  msgs <- Channel.receiveAll
  liftIO $ if Map.null msgs then return () else print msgs

  -- Get incoming messages and generate events

  -- Look at scene and generate events

  -- Update scene based on events and behaviour
  
  -- Update clients
  --liftIO $ putStrLn "Updating players"
  Channel.broadcast (FullUpdate scene)
  
  time <- liftIO Clock.getCurrentTime
  liftIO $ threadDelay (round ((0.5 - timeDiff time stepTime) * 1000000))
  tick scene stepTime

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

playerPosition :: Map WalkingKey UTCTime -> Vector3 (Vector3 Double) -> Vector3 Double -> Vector3 Double
playerPosition keys dir pos = pos + lv where
  up = vector3 0 1 0
  right = vx dir
  front = cross right up
  lv = rotate (vector3 right up front) (fmap (* 0.1) $ normalize $ Map.foldrWithKey (\k x y -> movementFromKey k + y) zero keys)

playerOrientation :: Vector2 Int -> Vector3 (Vector3 Double) -> Vector3 (Vector3 Double)
playerOrientation mouse dir = if vy (vy rotatedY) >= 0 then fmap rotX rotatedY else fmap rotX dir where
  rotX = rotateByAngle (fromIntegral (vx mouse) * (-0.001)) (vector3 0 1 0)
  rotY = rotateByAngle (fromIntegral (vy mouse) * (0.001)) (vx dir)
  rotatedY = fmap rotY dir

movementFromKey :: WalkingKey -> Vector3 Double
movementFromKey WalkLeft = vector3 1 0 0
movementFromKey WalkRight = vector3 (-1) 0 0
movementFromKey WalkDown = vector3 0 (-1) 0
movementFromKey WalkUp = vector3 0 1 0
movementFromKey WalkBackward = vector3 0 0 (-1)
movementFromKey WalkForward = vector3 0 0 1

-- Linear Algebra
type Angle = Double

degrees :: Angle -> Angle
degrees x = x / pi * 180

translate :: (Group a, Nat n) => Vector n a -> Vector n a -> Vector n a
translate = (+)

rotate :: (Nat n, Ring a) => Matrix n n a -> Vector n a -> Vector n a
rotate vs = (transpose vs *>)

rotateByAngle :: (Floating a, Ring a) => a -> Vector3 a -> Vector3 a -> Vector3 a
rotateByAngle a (Cons x (Cons y (Cons z Nil))) = rotate (vector3
  (vector3 (x * x * mc + c) (y * x * mc + z * s) (z * x * mc - y * s))
  (vector3 (x * y * mc - z * s) (y * y * mc + c) (z * y * mc + x * s))
  (vector3 (x * z * mc + y * s) (y * z * mc - x * s) (z * z * mc + c))) where
    c = cos a
    mc = 1 - c
    s = sin a

scale :: (Nat n, Ring a) => Vector n a -> Vector n a -> Vector n a
scale = zipWith (*)

uscale :: (Nat n, Ring a) => a -> Vector n a -> Vector n a
uscale = (*>)

-- Ray tracing
data RayTraceResult f a = Miss | Inside | Hit (a, f a) (a, f a) deriving (Eq, Ord, Show)

rayTraceAABB :: (DivisionRing a, Nat n, Ord a) => Vector n a -> Vector n a -> Vector2 (Vector n a) -> RayTraceResult (Vector n) a
rayTraceAABB pos dir (Cons low (Cons high Nil))
  | any (== Miss) dims = Miss
  | all (== Inside) dims = Inside
  | fst near > fst far = Miss
  | fst far < zero = Miss
  | otherwise = Hit near far where
    dims = zipWith5 rayTraceAABB1D pos dir low high one
    (nears, fars) = Prelude.unzip . hits . toList $ dims
    near = maximum nears
    far = minimum fars

rayTraceAABB1D :: (DivisionRing a, Ord a) => a -> a -> a -> a -> Vector n a -> RayTraceResult (Vector n) a
rayTraceAABB1D pos dir low high normal 
  | dir == zero = if pos < low || pos > high then Miss else Inside
  | otherwise = Hit near far where
    distX = (low - pos) / dir
    distY = (high - pos) / dir
    x = (distX, fmap negate normal)
    y = (distY, normal)
    near = if distX <= distY then x else y
    far = if distX > distY then x else y

hits :: [RayTraceResult f a] -> [((a, f a), (a, f a))]
hits [] = []
hits (Hit x y:xs) = (x, y) : hits xs
hits (_:xs) = hits xs

isHit :: RayTraceResult f a -> Bool
isHit (Hit _ _) = True
isHit _ = False

-- rayTraceBlocks :: Double -> Vector3 Double -> Vector3 Double -> Map (Vector3 Double) Block -> Maybe (Vector3 Double, RayTraceResult Vector3 Double)
-- rayTraceBlocks maxDist pos dir grid = if null sorted || nearDist > maxDist then Nothing else Just (P.head sorted) where
  -- aabbs = M.mapWithKey (\k x -> Cons k (Cons (k + repeat one) Nil)) grid
  -- rays = M.map (rayTraceAABB (uscale 5 pos) dir) aabbs
  -- hits = M.toList (M.filter isHit rays)
  -- sorted = sortBy (P.compare `on` \(_, Hit (dist, _) _) -> dist) hits
  -- (_, Hit (nearDist, _) _) = P.head sorted
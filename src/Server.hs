{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TemplateHaskell, TypeOperators #-}
module Server where

import Prelude hiding (id, (.), (+), (-), (*), (/), zipWith)
import Control.Category
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad
import Data.Label
import qualified Data.Time.Clock as Clock
import Data.Function
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

tick :: Scene -> Clock.UTCTime -> GameMonad ()
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

playerPosition :: Map.Map WalkingKey Clock.UTCTime -> Vector3 (Vector3 Double) -> Vector3 Double -> Vector3 Double
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
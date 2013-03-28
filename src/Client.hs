{-# LANGUAGE ScopedTypeVariables #-}
module Client where

import Prelude hiding ((.), id, Num (..))
import Control.Category
import qualified Data.Label as L
import Data.Label

import Data.List (foldl')
import Data.IORef
import Control.Monad
import qualified Graphics.UI.GLUT as GL
import Graphics.UI.GLUT hiding (KeyState, KeyUp, KeyDown, get, set)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Concurrent.MVar
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Monad.Reader
import Control.Monad.State (StateT, runStateT)
import qualified Control.Monad.State.Class as State

import qualified Data.Vector as V
import Data.Vector (vector2)
import Data.Algebra
import Network.Channel
import Common as Msg
import Common as Data
import Data.Client
import Graphics
import Data.Body as Body

main = withSockets $ do
  chan <- connect "localhost" 3000
  send chan Connect
  now <- getCurrentTime
  let sc = set (player (Ref 1)) newPlayer . set (playerKeys (Ref 1)) S.empty $ newScene 
  stateVar <- newMVar (State (Ref 1) sc 0)
  inputVar <- newMVar (0, now, [])
  keyVar <- newMVar S.empty
  initial
  centerMousePointer
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse chan inputVar keyVar)
  motionCallback $= Just (motion chan inputVar)
  passiveMotionCallback $= Just (motion chan inputVar)
  idleCallback $= Just (idle chan inputVar stateVar)
  displayCallback $= display stateVar
  mainLoop

centerMousePointer :: MonadIO m => m ()
centerMousePointer = do
  (mx, my) <- getCenterOfScreen
  liftIO (pointerPosition $= Position mx my)

getCenterOfScreen :: MonadIO m => m (GLint, GLint)
getCenterOfScreen = do
  Size w h <- liftIO (GL.get windowSize)
  return (w `div` 2, h `div` 2)

data Input = Input {
  inputId ::Id,
  inputTimeDelta :: TimeDelta,
  inputChange :: InputChange
  } deriving (Show, Read)

queueInput :: Channel i ClientMessage -> MVar (Id, UTCTime, [Input]) -> InputChange -> IO ()
queueInput chan inputVar change = do
  now <- getCurrentTime
  (prevId, prevTime, inputs) <- takeMVar inputVar
  let newId = succ prevId
  putMVar inputVar (newId, now, Input newId (diffTime now prevTime) change : inputs)
  when (diffTime now prevTime > 0.04) $ do
    print (Input newId (diffTime now prevTime) change)
  send chan (Ping newId change)

keyboardMouse :: Channel i ClientMessage -> MVar (Id, UTCTime, [Input]) -> MVar (Set WalkingKey) -> KeyboardMouseCallback
keyboardMouse _ _ _ (Char '\ESC') Down _ _ = leaveMainLoop
keyboardMouse chan inputVar keyVar key keyState _ _ = case M.lookup key keyMap of
  Nothing -> return ()
  Just k -> do
    keys <- takeMVar keyVar
    case (keyState, S.member k keys) of
      (Down, False) -> do
        queueInput chan inputVar (KeyChange KeyDown k)
        putMVar keyVar (S.insert k keys)
      (Up, True) -> do
        queueInput chan inputVar (KeyChange KeyUp k)
        putMVar keyVar (S.delete k keys)
      _ -> do
        putMVar keyVar keys

keyMap :: Map Key WalkingKey
keyMap = M.fromList [
  (Char 'w', WalkForward),
  (Char 's', WalkBackward),
  (Char 'a', WalkLeft),
  (Char 'd', WalkRight),
  (Char ' ', WalkUp),
  (Char 'c', WalkDown)
  ]

motion :: Channel i ClientMessage -> MVar (Id, UTCTime, [Input]) -> MotionCallback
motion chan inputVar (Position x y) = do
  (mx, my) <- getCenterOfScreen
  when (x /= mx || y /= my) $ do
    pointerPosition $= Position mx my
    let v = vector2 (fromIntegral (x - mx)) (fromIntegral (y - my))
    queueInput chan inputVar (MouseChange v)

testScene = set (player (Ref 1)) newPlayer . set (playerKeys (Ref 1)) S.empty $ newScene 

idle :: Channel ServerMessage ClientMessage -> MVar (Id, UTCTime, [Input]) -> MVar State -> IdleCallback
idle chan inputVar stateVar = do
  -- Send idle message to server.
  queueInput chan inputVar NoChange

  -- Calculate new scene.
  State ref sc sid <- takeMVar stateVar
  (iid, time, inputs) <- takeMVar inputVar

  let inputs' = takeWhile (\i -> inputId i > sid) inputs
  let sc' = processInputs ref inputs' sc
  let sid' = if null inputs' then sid else inputId (head inputs')

  putMVar inputVar (iid, time, inputs)
  putMVar stateVar (State ref sc' sid')

  -- Process messages received from server.
  --msgs <- receive chan
  --mapM_ (processMessage stateVar) msgs

  postRedisplay Nothing

processMessage :: MVar State -> ServerMessage -> IO ()
processMessage stateVar (ConnectSuccess ref sc) = do
  _ <- takeMVar stateVar
  putMVar stateVar (State ref sc 0)
processMessage stateVar (FullUpdate sc) = do
  --scene =: sc
  return ()

processInputs :: Ref Player -> [Input] -> Scene -> Scene
processInputs ref inputs sc = foldr (processInput ref) sc inputs

processInput :: Ref Player -> Input -> Scene -> Scene
processInput ref (Input ident delta change) sc = modify (player ref) (setPlayerVelocity keys) newScene where
  newScene = processInputChange ref change . modify (player ref) (updateBody delta) $ sc
  keys = get (playerKeys ref) newScene

processInputChange :: Ref Player -> InputChange -> Scene -> Scene
processInputChange ref NoChange sc = sc
processInputChange ref (KeyChange KeyDown key) sc = modify (playerKeys ref) (S.insert key) sc
processInputChange ref (KeyChange KeyUp key) sc = modify (playerKeys ref) (S.delete key) sc
processInputChange ref (MouseChange v) sc = modify (player ref) (turnPlayer v) sc
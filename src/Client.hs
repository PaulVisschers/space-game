{-# LANGUAGE ScopedTypeVariables #-}
module Client where

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Label as L

import Control.Monad
import qualified Graphics.UI.GLUT as GL
import Graphics.UI.GLUT
import qualified Data.Map as Map
import Data.IORef

import qualified Data.Vector as V
import qualified Network.Channel.Client as Chan
import Common as Msg
import Common as Data
import Data.Client
import Graphics

type Chan = Chan.Channel ServerMessage ClientMessage

main = do
  chan <- Chan.connect "localhost" 3000
  Chan.send Msg.Connect chan
  stateRef <- newIORef (State Nothing newScene)
  initial
  centerMousePointer
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse chan)
  motionCallback $= Just (motion chan)
  passiveMotionCallback $= Just (motion chan)
  idleCallback $= Just (idle chan stateRef)
  displayCallback $= display stateRef
  mainLoop
  Chan.close chan

centerMousePointer :: IO ()
centerMousePointer = do
  (mx, my) <- getCenterOfScreen
  pointerPosition $= Position mx my

getCenterOfScreen :: IO (GLint, GLint)
getCenterOfScreen = do
  Size w h <- get windowSize
  return (w `div` 2, h `div` 2)

keyboardMouse :: Chan -> KeyboardMouseCallback -- TODO: Use local state to limit number of messages
keyboardMouse chan (Char '\ESC') Down _ _ = leaveMainLoop
keyboardMouse chan key keyState _ _ = case Map.lookup key keyMap of
  Nothing -> return ()
  Just k -> case keyState of
    Down -> Chan.send (Msg.KeyDown k) chan
    Up -> Chan.send (Msg.KeyUp k) chan

keyMap = Map.fromList [
  (Char 'w', WalkForward),
  (Char 's', WalkBackward),
  (Char 'a', WalkLeft),
  (Char 'd', WalkRight),
  (Char ' ', WalkUp),
  (Char 'c', WalkDown)
  ]

motion :: Chan -> MotionCallback -- TODO: Maybe make compounded messages and only send them a couple of times a second.
motion chan (Position x y) = do
  (mx, my) <- getCenterOfScreen
  when (x /= mx || y /= my) $ do
    pointerPosition $= Position mx my
    Chan.send (Msg.MouseLook (V.vector2 (fromIntegral (x - mx)) (fromIntegral (y - my)))) chan

idle :: Chan -> IORef State -> IdleCallback
idle chan stateRef = do
  processMessages chan stateRef
  postRedisplay Nothing

processMessages :: Chan -> IORef State -> IO ()
processMessages chan stateRef = do
  st <- get stateRef
  msgs <- Chan.receive chan
  stateRef $= foldl (flip processMessage) st msgs  

processMessage :: ServerMessage -> State -> State
processMessage (ConnectSuccess key sc) _ = State (Just key) sc
processMessage (FullUpdate sc) st = L.set scene sc st
  

centerMouse :: IO ()
centerMouse = do
  (Size w h) <- get windowSize
  pointerPosition $= Position (w `div` 2) (h `div` 2)
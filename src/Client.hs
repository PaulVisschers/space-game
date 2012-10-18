{-# LANGUAGE ScopedTypeVariables #-}
module Client where

import Control.Monad
import qualified Graphics.UI.GLUT as GL
import Graphics.UI.GLUT
import qualified Data.Map as Map
import Data.IORef

import qualified Data.Vector as V
import qualified Network.Channel.Client as Chan
import Common as Msg
import Common as Data
import Graphics

type Chan = Chan.Channel ServerMessage ClientMessage

main = do
  chan <- Chan.connect "localhost" 3000
  sceneRef <- newIORef emptyScene
  initial
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse chan)
  motionCallback $= Just (motion chan)
  passiveMotionCallback $= Just (motion chan)
  idleCallback $= Just (idle chan sceneRef)
  displayCallback $= display sceneRef
  mainLoop

reshape :: ReshapeCallback
reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 60 (fromIntegral w / fromIntegral h) 0.02 200
  matrixMode $= Modelview 0

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
  Size w h <- get windowSize
  let mx = w `div` 2
  let my = h `div` 2
  when (x /= mx || y /= my) $ do
    pointerPosition $= Position mx my
    Chan.send (Msg.MouseLook (V.vector2 (fromIntegral (x - mx)) (fromIntegral (y - my)))) chan

idle :: Chan -> IORef Scene -> IdleCallback
idle chan sceneRef = do
  msgs <- Chan.receive chan
  if null msgs then return () else print msgs

  msgs <- Chan.receive chan

  when (not $ null msgs) $ do
    let FullUpdate scene = last msgs
    sceneRef $= scene

  postRedisplay Nothing

centerMouse :: IO ()
centerMouse = do
  (Size w h) <- get windowSize
  pointerPosition $= Position (w `div` 2) (h `div` 2)
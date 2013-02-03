{-# LANGUAGE ScopedTypeVariables #-}
module Client where

import Prelude hiding ((.), id, Num (..))
import Control.Category
import qualified Data.Label as L
import Data.Label.PureM

import Control.Monad
import qualified Graphics.UI.GLUT as GL
import Graphics.UI.GLUT hiding (KeyState, KeyUp, KeyDown)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Concurrent.MVar
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Monad.Reader
import Control.Monad.State (StateT, runStateT)
import qualified Control.Monad.State.Class as State

import qualified Data.Vector as V
import Data.Algebra
import Network.Channel.Client.Trans
import Common as Msg
import Common as Data
import Data.Client
import Graphics

type ClientMonad a = StateT State (ReaderT (Channel ServerMessage ClientMessage) IO) a

wrapCallback :: Channel ServerMessage ClientMessage -> MVar State -> ClientMonad a -> IO a
wrapCallback chan stateVar callback = do
  state <- takeMVar stateVar
  (x, s) <- withChannel chan (runStateT callback state)
  putMVar stateVar s
  return x

wrapCallback1 :: Channel ServerMessage ClientMessage -> MVar State -> (a -> ClientMonad b) -> a -> IO b
wrapCallback1 chan stateVar callback val = do
  state <- takeMVar stateVar
  (x, s) <- withChannel chan (runStateT (callback val) state)
  putMVar stateVar s
  return x

wrapCallback4 :: Channel ServerMessage ClientMessage -> MVar State -> (a -> b -> c -> d -> ClientMonad e) -> a -> b -> c -> d -> IO e
wrapCallback4 chan stateVar callback val1 val2 val3 val4 = do
  state <- takeMVar stateVar
  (x, s) <- withChannel chan (runStateT (callback val1 val2 val3 val4) state)
  putMVar stateVar s
  return x

main = do
  chan <- connect "localhost" 3000
  withChannel chan (send Msg.Connect)
  now <- getCurrentTime
  stateVar <- newMVar (State Nothing 0 now zero [] [] Set.empty newScene)
  initial
  centerMousePointer
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (wrapCallback4 chan stateVar keyboardMouse)
  motionCallback $= Just (wrapCallback1 chan stateVar motion)
  passiveMotionCallback $= Just (wrapCallback1 chan stateVar motion)
  idleCallback $= Just (wrapCallback chan stateVar idle)
  displayCallback $= display stateVar
  mainLoop
  close chan

centerMousePointer :: MonadIO m => m ()
centerMousePointer = do
  (mx, my) <- getCenterOfScreen
  liftIO (pointerPosition $= Position mx my)

getCenterOfScreen :: MonadIO m => m (GLint, GLint)
getCenterOfScreen = do
  Size w h <- liftIO (get windowSize)
  return (w `div` 2, h `div` 2)

keyboardMouse :: Key -> GL.KeyState -> Modifiers -> Position -> ClientMonad ()
keyboardMouse (Char '\ESC') Down _ _ = liftIO leaveMainLoop
keyboardMouse key keyState _ _ = case Map.lookup key keyMap of
  Nothing -> return ()
  Just k -> do
    now <- liftIO getCurrentTime
    tickStart <- gets currentTickStartTime
    let offset = diffTime now tickStart
    
    keys <- gets pressedKeys

    when (keyState == Down && not (Set.member k keys)) $ do
      pressedKeys =. Set.insert k
      currentTickKeyChanges =. (KeyChange offset KeyDown k :)
    when (keyState == Up && Set.member k keys) $ do
      pressedKeys =. Set.delete k
      currentTickKeyChanges =. (KeyChange offset KeyUp k :)

keyMap :: Map.Map Key WalkingKey
keyMap = Map.fromList [
  (Char 'w', WalkForward),
  (Char 's', WalkBackward),
  (Char 'a', WalkLeft),
  (Char 'd', WalkRight),
  (Char ' ', WalkUp),
  (Char 'c', WalkDown)
  ]

motion :: Position -> ClientMonad ()
motion (Position x y) = do
  (mx, my) <- getCenterOfScreen
  when (x /= mx || y /= my) $ do
    liftIO (pointerPosition $= Position mx my)
    currentTickMouseMovement =. (+ V.vector2 (fromIntegral (x - mx)) (fromIntegral (y - my)))

idle :: ClientMonad ()
idle = do
  -- Gather user input for this tick and bundle it.
  now <- liftIO getCurrentTime
  tickStart <- gets currentTickStartTime
  let duration = diffTime now tickStart

  idNum <- gets nextTickId
  mouse <- gets currentTickMouseMovement
  keys <- gets currentTickKeyChanges

  let userInput = UserInput idNum duration mouse keys
  liftIO (putStrLn ("Sending: " ++ show userInput))
  send (InputChanges userInput)

  nextTickId =. (+ 1)
  currentTickStartTime =: now
  currentTickMouseMovement =: zero
  currentTickKeyChanges =: []
  userInputs =. (userInput :)
  
  -- Process messages received from server.
  msgs <- receive
  mapM_ processMessage msgs

  liftIO (postRedisplay Nothing)

processMessage :: ServerMessage -> ClientMonad ()
processMessage (ConnectSuccess key sc) = do
  playerRef =: Just key
  scene =: sc
processMessage (FullUpdate sc) = do
  scene =: sc
{-# LANGUAGE ScopedTypeVariables #-}
module Client where

import Prelude hiding ((.), id, Num (..))
import Control.Category
import qualified Data.Label as L
import Data.Label

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
--import Network.Channel.Client.Trans as Chan
import Network.Channel.Client
import Common as Msg
import Common as Data
import Data.Client
import Graphics
import Data.Body as Body

type ClientMonad a = StateT State (ReaderT (Channel ServerMessage ClientMessage) IO) a

--wrapCallback :: Channel ServerMessage ClientMessage -> MVar State -> ClientMonad a -> IO a
--wrapCallback chan stateVar callback = do
--  state <- takeMVar stateVar
--  (x, s) <- withChannel chan (runStateT callback state)
--  putMVar stateVar s
--  return x

--wrapCallback1 :: Channel ServerMessage ClientMessage -> MVar State -> (a -> ClientMonad b) -> a -> IO b
--wrapCallback1 chan stateVar callback val = do
--  state <- takeMVar stateVar
--  (x, s) <- withChannel chan (runStateT (callback val) state)
--  putMVar stateVar s
--  return x

--wrapCallback4 :: Channel ServerMessage ClientMessage -> MVar State -> (a -> b -> c -> d -> ClientMonad e) -> a -> b -> c -> d -> IO e
--wrapCallback4 chan stateVar callback val1 val2 val3 val4 = do
--  state <- takeMVar stateVar
--  (x, s) <- withChannel chan (runStateT (callback val1 val2 val3 val4) state)
--  putMVar stateVar s
--  return x

main = do
  chan <- connect "localhost" 3000
  --send Connect chan
  now <- getCurrentTime
  let sc = set (player (Ref 1)) newPlayer . set (playerKeys (Ref 1)) S.empty $ newScene 
  stateVar <- newMVar (State (Just (Ref 1)) sc sc 0)
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
  close chan

centerMousePointer :: MonadIO m => m ()
centerMousePointer = do
  (mx, my) <- getCenterOfScreen
  liftIO (pointerPosition $= Position mx my)

getCenterOfScreen :: MonadIO m => m (GLint, GLint)
getCenterOfScreen = do
  Size w h <- liftIO (GL.get windowSize)
  return (w `div` 2, h `div` 2)

--keyboardMouse :: Key -> GL.KeyState -> Modifiers -> Position -> ClientMonad ()
--keyboardMouse (Char '\ESC') Down _ _ = liftIO leaveMainLoop
--keyboardMouse key keyState _ _ = case Map.lookup key keyMap of
--  Nothing -> return ()
--  Just k -> do
--    now <- liftIO getCurrentTime
--    lastKeyPress <- gets currentTickLastKeyPress
--    let duration = diffTime now lastKeyPress
--    currentTickLastKeyPress =: now
    
--    keys <- gets currentTickPressedKeys

--    when (keyState == Down && not (Set.member k keys)) $ do
--      currentTickPressedKeys =. Set.insert k
--      currentTickKeyChanges =. (KeyChange duration KeyDown k :)
--    when (keyState == Up && Set.member k keys) $ do
--      currentTickPressedKeys =. Set.delete k
--      currentTickKeyChanges =. (KeyChange duration KeyUp k :)

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

--motion :: Position -> ClientMonad ()
--motion (Position x y) = do
--  (mx, my) <- getCenterOfScreen
--  when (x /= mx || y /= my) $ do
--    liftIO (pointerPosition $= Position mx my)
--    currentTickMouseMovement =. (+ V.vector2 (fromIntegral (x - mx)) (fromIntegral (y - my)))

data Input = Input Id TimeDelta InputChange deriving (Show, Read)

motion :: Channel i ClientMessage -> MVar (Id, UTCTime, [Input]) -> MotionCallback
motion chan inputVar (Position x y) = do
  (mx, my) <- getCenterOfScreen
  when (x /= mx || y /= my) $ do
    pointerPosition $= Position mx my
    let v = vector2 (fromIntegral (x - mx)) (fromIntegral (y - my))
    queueInput chan inputVar (MouseChange v)

queueInput :: Channel i ClientMessage -> MVar (Id, UTCTime, [Input]) -> InputChange -> IO ()
queueInput chan inputVar change = do
  now <- getCurrentTime
  (prevId, prevTime, inputs) <- takeMVar inputVar
  let newId = succ prevId
  putMVar inputVar (newId, now, Input newId (diffTime now prevTime) change : inputs)
  putStrLn ("Queueing " ++ show (Input newId (diffTime now prevTime) change))
  send (Ping newId change) chan


testScene = set (player (Ref 1)) newPlayer . set (playerKeys (Ref 1)) S.empty $ newScene 

idle :: Channel ServerMessage ClientMessage -> MVar (Id, UTCTime, [Input]) -> MVar State -> IdleCallback
idle chan inputVar stateVar = do
  queueInput chan inputVar NoChange

  --State mref csc ssc sid <- readMVar stateVar
  (_, _ , inputs) <- readMVar inputVar

  --print mref
  let csc' = processInputs (Ref 1) inputs testScene
  --writeFile "log" (show csc')
  --seq csc' (return ())

  --State mref csc ssc sid <- takeMVar stateVar
  --case mref of
  --  Nothing -> putMVar stateVar (State mref csc' ssc sid)
  --  Just ref -> putMVar stateVar (State mref csc' ssc sid) --putMVar stateVar (State mref (processInputs ref inputs ssc) ssc sid)
  
  --print csc
  ---- Gather user input for this tick and bundle it.
  --now <- liftIO getCurrentTime
  --tickStart <- gets currentTickStartTime
  --let duration = diffTime now tickStart

  --idNum <- gets nextTickId
  --mouse <- gets currentTickMouseMovement
  --keys <- gets currentTickKeyChanges

  --let userInput = UserInput idNum duration mouse keys
  --liftIO (putStrLn ("Sending: " ++ show userInput))
  ----send (ClientUpdate userInput)

  --ref <- gets playerRef
  --case ref of
  --  Nothing -> return ()
  --  Just ref -> do
  --    processUserInput ref userInput
  --    --p <- gets (Body.linPos . player ref)
  --    --liftIO (print p)

  --nextTickId =. (+ 1)
  --currentTickStartTime =: now
  --currentTickLastKeyPress =: now
  --currentTickMouseMovement =: zero
  --currentTickKeyChanges =: []
  ----userInputs =. (userInput :)
  
  -- Process messages received from server.
  --msgs <- receive chan
  --mapM_ (processMessage stateVar) msgs

  postRedisplay Nothing

processMessage :: MVar State -> ServerMessage -> IO ()
processMessage stateVar (ConnectSuccess key sc) = do
  _ <- takeMVar stateVar
  putMVar stateVar (State (Just key) sc sc 0)
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
processInputChange ref (MouseChange v) sc = sc -- modify (angPos . player ref) (playerOrientation v) sc -- NOTE: playerOrientation does not take turning speed of a player into account (and name is confusing).
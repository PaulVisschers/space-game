{-# LANGUAGE GADTs, TypeOperators, FlexibleContexts, RankNTypes, NoMonomorphismRestriction #-}

import qualified Prelude as P
import Prelude hiding ((+), (-), negate, (*), (/), recip, repeat, minimum, maximum, any, all, zipWith)
import Data.Foldable
import Control.Monad
import Data.List (sortBy)
import Data.Function (on)

import Data.IORef
import qualified Graphics.UI.GLUT as GL
import Graphics.UI.GLUT (HasGetter (..), HasSetter (..), ($~))
import qualified Data.Map as M
import Data.Map (Map)
import Foreign (newArray)
import Data.Either
import Data.Maybe
import Data.Time.Clock

import Data.Vector
import Data.Algebra
import Data.Record
import BlockGrid

-- StateVar
data StateVar a where
  IOVar :: IORef a -> StateVar a
  RefVar :: HasField l s a => l -> StateVar (Record s) -> StateVar a

newStateVar :: a -> IO (StateVar a)
newStateVar = fmap IOVar . newIORef

(!) :: HasField l s a => StateVar (Record s) -> l -> StateVar a
v ! l = RefVar l v
infixl 5 !

instance HasGetter (StateVar) where
  get (IOVar r) = readIORef r
  get (RefVar l v) = fmap (getField l) (get v)

instance HasSetter (StateVar) where
  v $= x = modify v (const x) where
    modify (IOVar r) f = modifyIORef r f
    modify (RefVar l v) f = v $~ (modifyField l f)

copy :: (HasGetter f, HasSetter g) => f a -> g a -> IO ()
copy v1 v2 = get v1 >>= (v2 $=)

copyWith :: (HasGetter f, HasSetter g) => (a -> b) -> f a -> g b -> IO ()
copyWith f v1 v2 = get v1 >>= (v2 $=) . f

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

uscale :: (Nat n, Ring a) => a -> Vector n a -> Vector n a
uscale = (*>)

scale :: (Nat n, Ring a) => Vector n a -> Vector n a -> Vector n a
scale = zipWith (*)

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
    (nears, fars) = P.unzip . hits . toList $ dims
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

-- OpenGL functions that work with Vectors
glConvert2 :: (Double -> Double -> f Double) -> Vector2 Double -> f Double
glConvert2 f (Cons x (Cons y Nil)) = f x y

glConvert3 :: (Double -> Double -> Double -> f Double) -> Vector3 Double -> f Double
glConvert3 f (Cons x (Cons y (Cons z Nil))) = f x y z

glLookAt :: Vector3 Double -> Vector3 Double -> Vector3 Double -> IO ()
glLookAt p df du = GL.lookAt (glConvert3 GL.Vertex3 p) (glConvert3 GL.Vertex3 df) (glConvert3 GL.Vector3 du)

glTranslate :: Vector3 Double -> IO ()
glTranslate = GL.translate . glConvert3 GL.Vector3

glVertex :: Vector3 Double -> IO ()
glVertex = GL.vertex . glConvert3 GL.Vertex3

glNormal :: Vector3 Double -> IO ()
glNormal = GL.normal . glConvert3 GL.Normal3

glColor :: Vector3 Double -> IO ()
glColor = GL.color . glConvert3 GL.Color3

glTexCoord :: Vector2 Double -> IO ()
glTexCoord = GL.texCoord . glConvert2 GL.TexCoord2

glScale :: Vector3 Double -> IO ()
glScale (Cons x (Cons y (Cons z Nil))) = GL.scale x y z

glUScale :: Double -> IO ()
glUScale x = GL.scale x x x

-- Record type and label declarations
type Settings = Record (RCons KeyBindingsLabel (Map GL.Key Key) (RCons WindowSizeLabel (Vector2 Int) RNil))
data KeyBindingsLabel = KeyBindings
data WindowSizeLabel = WindowSize

type Input = Record (RCons KeysPressedLabel (Map Key UTCTime) (RCons MouseLookLabel (Vector2 Int) RNil))
data KeysPressedLabel = KeysPressed
data MouseLookLabel = MouseLook

type Scene = Record (RCons PlayerLabel Player (RCons GridLabel BlockGrid (RCons ConstructsLabel [Construct] RNil)))
data PlayerLabel = Player
data GridLabel = Grid
data ConstructsLabel = Constructs

type Player = Record (RCons PositionLabel (Vector3 Double) (RCons OrientationLabel (Vector3 (Vector3 Double)) (RCons ViewLocationLabel (Maybe (Vector3 Double)) (RCons PlacementLocationLabel (Vector3 Double) RNil))))
data PositionLabel = Position
data OrientationLabel = Orientation
data ViewLocationLabel = ViewLocation
data PlacementLocationLabel = PlacementLocation

blockSize = 0.2
type Construct = Record (RCons PositionLabel (Vector3 Double) (RCons OrientationLabel (Vector3 (Vector3 Double)) (RCons BlocksLabel (Map (Vector3 Int) Block) RNil)))
data BlocksLabel = Blocks

type State = Record (RCons SettingsLabel Settings (RCons InputLabel Input (RCons SceneLabel Scene (RCons CurrentTimeLabel UTCTime RNil))))
data SettingsLabel = Settings
data InputLabel = Input
data SceneLabel = Scene
data CurrentTimeLabel = CurrentTime

-- Non-record data types
data Key = StrafeLeft | StrafeRight | Downward | Upward | Forward | Backward | PlaceBlock | PickBlock | ExitGame deriving (Eq, Ord, Show)

defaultSettings :: Settings
defaultSettings = RCons kb (RCons ws RNil) where
  kb = M.fromList [
    (GL.Char 'a', StrafeLeft),
    (GL.Char 'd', StrafeRight),
    (GL.Char 'c', Downward),
    (GL.Char ' ', Upward),
    (GL.Char 'w', Forward),
    (GL.Char 's', Backward),
    (GL.MouseButton GL.LeftButton, PickBlock),
    (GL.MouseButton GL.RightButton, PlaceBlock),
    (GL.Char '\ESC', ExitGame)
    ]
  ws = vector2 1920 1080

initialInput :: Input
initialInput = RCons pk (RCons ml RNil) where
  pk = M.empty
  ml = zero

initialScene :: Scene
initialScene = RCons p (RCons g (RCons bos RNil)) where
  p = newPlayer
  g = emptyGrid
  bos = []

newPlayer :: Player
newPlayer = RCons pos (RCons dir (RCons viewLocation (RCons placementLocation RNil))) where
  pos = vector3 0 0 (-5)
  dir = vector3 (vector3 1 0 0) (vector3 0 1 0) (vector3 0 0 1)
  viewLocation = Nothing
  placementLocation = pos + vz dir

initialState :: UTCTime -> State
initialState time = RCons se (RCons i (RCons sc (RCons time RNil))) where
  se = defaultSettings
  i = initialInput
  sc = initialScene

-- Main code
main :: IO ()
main = do
  t <- getCurrentTime
  s <- newStateVar (initialState t)
  initial (s ! Settings)
  GL.reshapeCallback $= Just (reshape (s ! Settings))
  GL.keyboardMouseCallback $= Just (button (s ! Settings ! KeyBindings) (s ! Input ! KeysPressed))
  GL.motionCallback $= Just (motion (s ! Input ! MouseLook))
  GL.passiveMotionCallback $= Just (motion (s ! Input ! MouseLook))
  GL.idleCallback $= Just (idle s)
  GL.displayCallback $= display (s ! Scene)
  GL.mainLoop

initial :: StateVar Settings -> IO ()
initial state = do
  GL.getArgsAndInitialize
  GL.initialDisplayMode $= [GL.RGBAMode, GL.DoubleBuffered, GL.WithDepthBuffer, GL.Multisampling]
  --GL.initialWindowSize $= GL.Size 1600 900 -- Read from state.
  --GL.initialWindowPosition $= GL.Position 100 50 -- Maybe also read from state.
  window <- GL.createWindow "Spacey Space"
  GL.fullScreen

  GL.clearColor $= GL.Color4 0 0 0 0
  GL.shadeModel $= GL.Smooth
  GL.materialAmbient GL.Front $= GL.Color4 1 1 1 1
  GL.materialDiffuse GL.Front $= GL.Color4 1 1 1 1
  GL.materialSpecular GL.Front $= GL.Color4 1 1 1 1
  GL.materialEmission GL.Front $= GL.Color4 0 0 0 1
  GL.materialShininess GL.Front $= 50

  --GL.position (GL.Light 0) $= GL.Vertex4 0 0 0 1
  GL.diffuse (GL.Light 0) $= GL.Color4 0.5 0.5 0.5 1
  GL.specular (GL.Light 0) $= GL.Color4 0 0 0 1
  GL.lightModelAmbient $= GL.Color4 0.2 0.2 0.2 1

  GL.lighting $= GL.Enabled
  GL.light (GL.Light 0) $= GL.Enabled
  GL.depthFunc $= Just GL.Less
  GL.multisample $= GL.Enabled
  GL.fog $= GL.Enabled
  GL.fogMode $= GL.Exp2 0.05
  GL.fogColor $= GL.Color4 0 0 0 1

  -- Texturing
  GL.rowAlignment GL.Unpack $= 1
  textureName <- fmap P.head (GL.genObjectNames 1)
  GL.textureBinding GL.Texture2D $= Just textureName
  let oddLine = P.concat (P.replicate 32 ([255, 0 :: GL.GLubyte] >>= P.replicate 32))
  let evenLine = P.concat (P.replicate 32 ([0, 255 :: GL.GLubyte] >>= P.replicate 32))
  let lines = oddLine ++ evenLine
  textureData <- newArray (fmap (\n -> GL.Color4 n n n 127) lines)
  
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Clamp)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Clamp)
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  
  GL.texture GL.Texture2D $= GL.Enabled
  GL.textureFunction $= GL.Blend
  
  GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA'(GL.TextureSize2D 64 64) 0 (GL.PixelData GL.RGBA GL.UnsignedByte textureData)

  --GL.cursor $= GL.None
  GL.pointerPosition $= GL.Position 800 450

reshape :: StateVar Settings -> GL.ReshapeCallback
reshape state s@(GL.Size w h) = do
  GL.viewport $= (GL.Position 0 0, s)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  --GL.frustum (-1) 1 (-1) 1 1.5 20
  GL.perspective 60 (P.fromIntegral w / P.fromIntegral h) 0.02 200
  GL.matrixMode $= GL.Modelview 0

button :: StateVar (Map GL.Key Key) -> StateVar (Map Key UTCTime) -> GL.KeyboardMouseCallback
button keyBindings pressedKeys glKey keyState _ _ = do
  time <- getCurrentTime
  kb <- get keyBindings
  pressedKeys $~ updatePressedKeys (addUTCTime (-1) time) kb glKey keyState

updatePressedKeys time keyBindings glKey keyState = case M.lookup glKey keyBindings of
    Nothing -> id
    Just k -> case keyState of
      GL.Down -> M.insertWith const k time
      GL.Up -> M.delete k

motion :: StateVar (Vector2 Int) -> GL.MotionCallback
motion mouseLook (GL.Position x y) = do
  -- TODO: Use windowSize
  let mx = 960
  let my = 540
  when (x /= mx || y /= my) (GL.pointerPosition $= GL.Position mx my)
  mouseLook $~ (+ vector2 (fromIntegral (x - mx)) (fromIntegral (y - my)))

idle :: StateVar State -> GL.IdleCallback
idle state = do
  let input = state ! Input
  let scene = state ! Scene
  
  oldTime <- get (state ! CurrentTime)
  time <- getCurrentTime
  let dt = fromRational (toRational (diffUTCTime oldTime time))
  state ! CurrentTime $= time

  let player = scene ! Player
  pos <- get (player ! Position)

  -- Moves player by key presses
  keys <- get (input ! KeysPressed)
  dir <- get (player ! Orientation)
  (player ! Position) $~ playerPosition keys dir

  -- Rotates player by mouse movements
  mouse <- get (input ! MouseLook)
  (player ! Orientation) $~ playerOrientation mouse
  input ! MouseLook $= vector2 0 0

  -- Determines the coordinates of the block that is looked at, and where a new block is to be placed.
  pos <- get (player ! Position)
  dir <- get (player ! Orientation)
  grid <- get (scene ! Grid)
  let mplace = rayTraceBlocks 100 pos (vz dir) grid
  case mplace of
    Nothing -> do
      player ! ViewLocation $= Nothing
      player ! PlacementLocation $= snap (uscale 5 (pos + vz dir))
    Just (v, Hit (_, normal) _) -> do
      player ! ViewLocation $= Just v
      player ! PlacementLocation $= v + normal

  -- Places and picks blocks from grid
  let grid = scene ! Grid
  view <- get (player ! ViewLocation)
  placement <- get (player ! PlacementLocation)
  when (M.member PickBlock keys && isJust view && diffUTCTime time (keys M.! PickBlock) >= 0.25) $ do
    grid $~ setBlock (fromJust view) NoBlock
    (input ! KeysPressed) $~ M.insert PickBlock time
  when (M.member PlaceBlock keys && diffUTCTime time (keys M.! PlaceBlock) >= 0.25) $ do
    grid $~ setBlock placement TestBlock
    (input ! KeysPressed) $~ M.insert PlaceBlock time

  -- Exit game if requested
  when (M.member ExitGame keys) $ do
    GL.leaveMainLoop

  GL.postRedisplay Nothing

playerPosition :: Map Key UTCTime -> Vector3 (Vector3 Double) -> Vector3 Double -> Vector3 Double
playerPosition keys dir pos = pos + lv where
  up = vector3 0 1 0
  right = vx dir
  front = cross right up
  lv = rotate (vector3 right up front) (fmap (* 0.1) $ normalize $ M.foldrWithKey (\k x y -> movementFromKey k + y) zero keys)

playerOrientation :: Vector2 Int -> Vector3 (Vector3 Double) -> Vector3 (Vector3 Double)
playerOrientation mouse dir = if vy (vy rotatedY) >= 0 then fmap rotX rotatedY else fmap rotX dir where
  rotX = rotateByAngle (fromIntegral (vx mouse) * (-0.001)) (vector3 0 1 0)
  rotY = rotateByAngle (fromIntegral (vy mouse) * (0.001)) (vx dir)
  rotatedY = fmap rotY dir

movementFromKey :: Key -> Vector3 Double
movementFromKey StrafeLeft = vector3 1 0 0
movementFromKey StrafeRight = vector3 (-1) 0 0
movementFromKey Downward = vector3 0 (-1) 0
movementFromKey Upward = vector3 0 1 0
movementFromKey Backward = vector3 0 0 (-1)
movementFromKey Forward = vector3 0 0 1
movementFromKey _ = zero

rayTraceBlocks :: Double -> Vector3 Double -> Vector3 Double -> Map (Vector3 Double) Block -> Maybe (Vector3 Double, RayTraceResult Vector3 Double)
rayTraceBlocks maxDist pos dir grid = if null sorted || nearDist > maxDist then Nothing else Just (P.head sorted) where
  aabbs = M.mapWithKey (\k x -> Cons k (Cons (k + repeat one) Nil)) grid
  rays = M.map (rayTraceAABB (uscale 5 pos) dir) aabbs
  hits = M.toList (M.filter isHit rays)
  sorted = sortBy (P.compare `on` \(_, Hit (dist, _) _) -> dist) hits
  (_, Hit (nearDist, _) _) = P.head sorted



display :: StateVar Scene -> GL.DisplayCallback
display scene = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.loadIdentity

  player <- get (scene ! Player)
  playerView player
  drawAxes
  grid <- get (scene ! Grid)
  renderGrid grid

  -- Debug wireframe at placement location
  pos <- get (scene ! Player ! Position)
  dir <- get (scene ! Player ! Orientation)

  view <- get (scene ! Player ! ViewLocation)
  placement <- get (scene ! Player ! PlacementLocation)

  when (isJust view) $ do
    GL.preservingMatrix $ do
      glUScale 0.2
      glTranslate (fromJust view)
      glColor (vector3 1 0 0)
      GL.lighting $= GL.Disabled
      GL.texture GL.Texture2D $= GL.Disabled
      glTranslate (vector3 0.5 0.5 0.5)
      GL.renderObject GL.Wireframe (GL.Cube 1)
      GL.lighting $= GL.Enabled
      GL.texture GL.Texture2D $= GL.Enabled
  GL.preservingMatrix $ do
    glUScale 0.2
    glTranslate placement
    glColor (vector3 0 1 0)
    GL.lighting $= GL.Disabled
    GL.texture GL.Texture2D $= GL.Disabled
    glTranslate (vector3 0.5 0.5 0.5)
    GL.renderObject GL.Wireframe (GL.Cube 1)
    GL.lighting $= GL.Enabled
    GL.texture GL.Texture2D $= GL.Enabled


  GL.position (GL.Light 0) $= GL.Vertex4 0 0 0 1
  GL.swapBuffers

playerView :: Player -> IO ()
playerView player = glLookAt pos (pos + front) up where
  pos = getField Position player
  front = vz (getField Orientation player)
  up = vy (getField Orientation player)

renderGrid :: BlockGrid -> IO ()
renderGrid = mapMWithKey_ renderBlock

renderBlock :: Vector3 Double -> Block -> IO ()
renderBlock pos bt = GL.preservingMatrix $ do
  glUScale 0.2
  glTranslate pos
  GL.renderPrimitive GL.TriangleStrip $ do
    -- Front
    glNormal (vector3 0 0 (-1))
    glTexCoord (vector2 0 0)
    glVertex (vector3 1 0 0)
    glTexCoord (vector2 0 1)
    glVertex (vector3 1 1 0)
    glTexCoord (vector2 1 0)
    glVertex (vector3 0 0 0)
    glTexCoord (vector2 1 1)
    glVertex (vector3 0 1 0)
  GL.renderPrimitive GL.TriangleStrip $ do
    -- Top
    glNormal (vector3 0 1 0)
    glTexCoord (vector2 0 0)
    glVertex (vector3 1 1 0)
    glTexCoord (vector2 1 0)
    glVertex (vector3 0 1 0)
    glTexCoord (vector2 0 1)
    glVertex (vector3 1 1 1)
    glTexCoord (vector2 1 1)
    glVertex (vector3 0 1 1)
  GL.renderPrimitive GL.TriangleStrip $ do
    -- Back
    glNormal (vector3 0 0 1)
    glTexCoord (vector2 0 0)
    glVertex (vector3 0 1 1)
    glTexCoord (vector2 1 0)
    glVertex (vector3 1 1 1)
    glTexCoord (vector2 0 1)
    glVertex (vector3 0 0 1)
    glTexCoord (vector2 1 1)
    glVertex (vector3 1 0 1)
  GL.renderPrimitive GL.TriangleStrip $ do
    -- Bottom
    glNormal (vector3 0 (-1) 0)
    glTexCoord (vector2 0 0)
    glVertex (vector3 0 0 0)
    glTexCoord (vector2 1 0)
    glVertex (vector3 1 0 0)
    glTexCoord (vector2 0 1)
    glVertex (vector3 0 0 1)
    glTexCoord (vector2 1 1)
    glVertex (vector3 1 0 1)
  GL.renderPrimitive GL.TriangleStrip $ do
    -- Left
    glNormal (vector3 1 0 0)
    glTexCoord (vector2 0 0)
    glVertex (vector3 1 0 1)
    glTexCoord (vector2 1 0)
    glVertex (vector3 1 0 0)
    glTexCoord (vector2 0 1)
    glVertex (vector3 1 1 1)
    glTexCoord (vector2 1 1)
    glVertex (vector3 1 1 0)
  GL.renderPrimitive GL.TriangleStrip $ do
    -- Right
    glNormal (vector3 (-1) 0 0)
    glTexCoord (vector2 0 0)
    glVertex (vector3 0 0 0)
    glTexCoord (vector2 1 0)
    glVertex (vector3 0 0 1)
    glTexCoord (vector2 0 1)
    glVertex (vector3 0 1 0)
    glTexCoord (vector2 1 1)
    glVertex (vector3 0 1 1)

drawAxes = do
  GL.lighting $= GL.Disabled
  GL.texture GL.Texture2D $= GL.Disabled
  GL.renderPrimitive GL.Lines $ do
    glColor (vector3 1 0 0)
    glVertex (vector3 0 0 0)
    glVertex (vector3 1 0 0)
  GL.renderPrimitive GL.Lines $ do
    glColor (vector3 0 1 0)
    glVertex (vector3 0 0 0)
    glVertex (vector3 0 1 0)
  GL.renderPrimitive GL.Lines $ do
    glColor (vector3 0 0 1)
    glVertex (vector3 0 0 0)
    glVertex (vector3 0 0 1)
  GL.lighting $= GL.Enabled
  GL.texture GL.Texture2D $= GL.Enabled

{-
motion windowSize mouseLook (GL.Position x y) = do
  when (x /= 800 || y /= 450) (GL.pointerPosition $= GL.Position 800 450)
  p <- get player
  let dir = direction p
  let rotX = rotateByAngle ((P.fromIntegral x - 800) * (-0.001)) (vector3 0 1 0)
  let rotY = rotateByAngle ((P.fromIntegral y - 450) * (0.001)) (vx dir)
  let rotatedY = fmap rotY dir
  let rotatedXY = if vy (vy rotatedY) >= 0 then fmap rotX rotatedY else fmap rotX dir
  player $= p {direction = rotatedXY}



{-
-- This version uses full movement over all 3 axes.
updatePlayer :: S.Set MovementKey -> Player -> Player
updatePlayer keys player = player {position = p', directionFront = df', directionUp = du'} where
  p = position player
  df = directionFront player
  du = directionUp player
  --av = (S.fold ((+) . keyToAngVel) zero keys) 
  --d = fmap normalizeAngle (direction player + ((0.05 :: Double) *> av))
  lv = fmap (* 0.1) (S.fold ((+) . keyToLinVel) zero keys)
  p' = transform (translate p * rotateFU df du) lv
  av = fmap (* 0.025) (S.fold ((+) . keyToAngVel) zero keys)
  df' = transform (rot df du av) df
  du' = transform (rot df du av) du
-}

updatePlayer keys player = player {position = p'} where
  p = position player
  up = vector3 0 1 0
  right = vx (direction player)
  front = cross right up
  lv = rotate (vector3 right up front) (fmap (* 0.1) $ normalize $ S.fold ((+) . movementFromKey) zero keys)
  p' = p + lv

updateGrid keys grid = 
  
--blaat = withVector3 $ \p y r -> rotate r (vector3 0 0 1) * rotate p (vector3 1 0 0) * rotate y (vector3 0 1 0)

--rot df du = withVector3 $ \p y r -> rotate r df * rotate p (cross df du) * rotate y du

--t1 = rot (vector3 1 2 3) (vector3 4 5 6) (vector3 (pi/4) (pi/2) (pi/8)) *> vector4 0 0 1 (1 :: Double)
--t2 = (rotateFU (vector3 1 2 3) (vector3 4 5 6) * blaat (vector3 (pi/4) (pi/2) (pi/8))) *> vector4 0 0 1 (1 :: Double)
{-
normalizeAngle a
  | a >= 2 * pi = normalizeAngle (a - 2 * pi)
  | a < 0 = normalizeAngle (a + 2 * pi)
  | otherwise = a
-}

{-
keyToAngVel :: MovementKey -> Vector3 Angle
keyToAngVel PitchDown = vector3 (-1) 0 0
keyToAngVel PitchUp = vector3 1 0 0
keyToAngVel YawLeft = vector3 0 1 0
keyToAngVel YawRight = vector3 0 (-1) 0
keyToAngVel RollLeft = vector3 0 0 (-1)
keyToAngVel RollRight = vector3 0 0 1
keyToAngVel _ = zero
-}








blockLine :: Int -> IO ()
blockLine n = GL.preservingMatrix (replicateM_ n block) where
  block = renderCube >> glTranslate (vector3 1 0 0)

toHomo :: Ring a => Vector n a -> Vector (Succ n) a
toHomo = snoc one

fromHomo :: DivisionRing b => Vector (Succ n) b -> Vector n b
fromHomo v = fmap (/ last v) (init v)

translationMatrix :: (Nat n, Ring a) => Vector n a -> Matrix (Succ n) (Succ n) a
translationMatrix v = transpose (snoc (snoc one v) (fmap (snoc zero) one))

rotationMatrix :: (Nat n, Ring a) => Vector n (Vector n a) -> Matrix (Succ n) (Succ n) a
rotationMatrix v = transpose (snoc (snoc one zero) (fmap (snoc zero) v))



type Angle = Double

degrees :: Angle -> Angle
degrees x = x / pi * 180

glRotate :: Angle -> Vector3 Double -> IO ()
glRotate a v = GL.rotate (degrees a) (glConvert3 GL.Vector3 v)





renderCube = do
  --glColor (vector3 1 1 1)
  GL.lighting $= GL.Enabled
  GL.texture GL.Texture2D $= GL.Enabled
  GL.renderPrimitive GL.TriangleStrip $ do
    -- Front
    glNormal (vector3 0 0 (-1))
    glTexCoord (vector2 0 0)
    glVertex (vector3 0 0 0)
    glTexCoord (vector2 1 0)
    glVertex (vector3 1 0 0)
    glTexCoord (vector2 0 1)
    glVertex (vector3 0 1 0)
    glTexCoord (vector2 1 1)
    glVertex (vector3 1 1 0)
  GL.renderPrimitive GL.TriangleStrip $ do
    -- Top
    glNormal (vector3 0 1 0)
    glTexCoord (vector2 0 0)
    glVertex (vector3 1 1 0)
    glTexCoord (vector2 1 0)
    glVertex (vector3 0 1 0)
    glTexCoord (vector2 0 1)
    glVertex (vector3 1 1 1)
    glTexCoord (vector2 1 1)
    glVertex (vector3 0 1 1)
  GL.renderPrimitive GL.TriangleStrip $ do
    -- Back
    glNormal (vector3 0 0 1)
    glTexCoord (vector2 0 0)
    glVertex (vector3 0 1 1)
    glTexCoord (vector2 1 0)
    glVertex (vector3 1 1 1)
    glTexCoord (vector2 0 1)
    glVertex (vector3 0 0 1)
    glTexCoord (vector2 1 1)
    glVertex (vector3 1 0 1)
  GL.renderPrimitive GL.TriangleStrip $ do
    -- Bottom
    glNormal (vector3 0 (-1) 0)
    glTexCoord (vector2 0 0)
    glVertex (vector3 0 0 0)
    glTexCoord (vector2 1 0)
    glVertex (vector3 1 0 0)
    glTexCoord (vector2 0 1)
    glVertex (vector3 0 0 1)
    glTexCoord (vector2 1 1)
    glVertex (vector3 1 0 1)
  GL.renderPrimitive GL.TriangleStrip $ do
    -- Left
    glNormal (vector3 1 0 0)
    glTexCoord (vector2 0 0)
    glVertex (vector3 1 0 1)
    glTexCoord (vector2 1 0)
    glVertex (vector3 1 0 0)
    glTexCoord (vector2 0 1)
    glVertex (vector3 1 1 1)
    glTexCoord (vector2 1 1)
    glVertex (vector3 1 1 0)
  GL.renderPrimitive GL.TriangleStrip $ do
    -- Right
    glNormal (vector3 (-1) 0 0)
    glTexCoord (vector2 0 0)
    glVertex (vector3 0 0 0)
    glTexCoord (vector2 1 0)
    glVertex (vector3 0 0 1)
    glTexCoord (vector2 0 1)
    glVertex (vector3 0 1 0)
    glTexCoord (vector2 1 1)
    glVertex (vector3 0 1 1)
  GL.lighting $= GL.Disabled
  GL.texture GL.Texture2D $= GL.Disabled


-}
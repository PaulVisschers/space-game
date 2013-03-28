{-# LANGUAGE GADTs #-}
module Graphics where

import Prelude hiding (Num (..), Fractional (..), (.), id)
import Control.Category
import qualified Data.Label as L
import Data.Map as M

import Graphics.UI.GLUT hiding (lookAt, translate, vertex, normal, color, texCoord, scale, Vector2, Vector3, position, rotate)
import qualified Graphics.UI.GLUT as GL
import Foreign (newArray)

import Data.IORef
import Data.LinearAlgebra hiding ((++), rotate, translate, uscale,)
import Common
import Data.Body
import Data.Client
import Control.Concurrent.MVar (MVar, readMVar)
import Data.BlockObject

initial :: IO ()
initial = do
  getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer, Multisampling]
  window <- createWindow "Spacey Space"
  fullScreen

  clearColor $= Color4 0 0 0 0

  -- Lighting
  shadeModel $= Smooth
  materialAmbient Front $= Color4 1 1 1 1
  materialDiffuse Front $= Color4 1 1 1 1
  materialSpecular Front $= Color4 1 1 1 1
  materialEmission Front $= Color4 0 0 0 1
  materialShininess Front $= 50

  diffuse (Light 0) $= Color4 0.5 0.5 0.5 1
  specular (Light 0) $= Color4 0 0 0 1
  lightModelAmbient $= Color4 0.2 0.2 0.2 1

  lighting $= Enabled
  light (Light 0) $= Enabled
  depthFunc $= Just Less
  multisample $= Enabled
  fog $= Enabled
  fogMode $= Exp2 0.05
  fogColor $= Color4 0 0 0 1

  -- Texturing
  rowAlignment Unpack $= 1
  textureName <- fmap Prelude.head (genObjectNames 1)
  textureBinding Texture2D $= Just textureName
  let oddLine = concat (replicate 32 ([255, 0 :: GLubyte] >>= replicate 32))
  let evenLine = concat (replicate 32 ([0, 255 :: GLubyte] >>= replicate 32))
  let lines = oddLine ++ evenLine
  textureData <- newArray (fmap (\n -> Color4 n n n 127) lines)
  
  textureWrapMode Texture2D S $= (Repeated, Clamp)
  textureWrapMode Texture2D T $= (Repeated, Clamp)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  
  texture Texture2D $= Enabled
  textureFunction $= Blend
  
  texImage2D Nothing NoProxy 0 RGBA'(TextureSize2D 64 64) 0 (PixelData RGBA UnsignedByte textureData)
  --cursor $= None

display :: MVar State -> DisplayCallback
display stateRef = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  state <- readMVar stateRef
  let ref = L.get playerRef state
  let playerPosition = L.get (position . item ref . players . scene) state
  --setView (L.get (position . spatial) newPlayer)
  setView playerPosition
  --putStrLn ("setting view to " ++ show (L.get linear playerPosition))
  --putStrLn ""

  drawAxes

  let bos = L.get (blockObjects . scene) state
  renderBlockObjects bos

  renderBlockObjectWireFrames playerPosition bos

  --let val = 2
  --renderBlock (vector3 val 0 0)
  --renderBlock (vector3 (-val) 0 0)
  --renderBlock (vector3 0 val 0)
  --renderBlock (vector3 0 (-val) 0)
  --renderBlock (vector3 0 0 val)
  --renderBlock (vector3 0 0 (-val))
  --grid <- get (scene ! Grid)
  --renderGrid grid
  {-
  -- Debug wireframe at placement location
  --pos <- get (scene ! Player ! Position)
  --dir <- get (scene ! Player ! Orientation)

  --view <- get (scene ! Player ! ViewLocation)
  --placement <- get (scene ! Player ! PlacementLocation)

  when (isJust view) $ do
    preservingMatrix $ do
      uscale 0.2
      translate (fromJust view)
      color (vector3 1 0 0)
      lighting $= Disabled
      texture Texture2D $= Disabled
      translate (vector3 0.5 0.5 0.5)
      renderObject Wireframe (Cube 1)
      lighting $= Enabled
      texture Texture2D $= Enabled
  preservingMatrix $ do
    uscale 0.2
    translate placement
    color (vector3 0 1 0)
    lighting $= Disabled
    texture Texture2D $= Disabled
    translate (vector3 0.5 0.5 0.5)
    renderObject Wireframe (Cube 1)
    lighting $= Enabled
    texture Texture2D $= Enabled
  -}

  GL.position (Light 0) $= Vertex4 0 0 0 1
  swapBuffers

reshape :: ReshapeCallback
reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 60 (fromIntegral w / fromIntegral h) 0.02 200
  matrixMode $= Modelview 0

setView :: Components -> IO ()
setView comps = lookAt pos (pos + front) up where
  pos = L.get linear comps
  front = vz $ L.get angular comps
  up = vy $ L.get angular comps

renderBlockObjectWireFrames :: Components -> Map (Ref BlockObject) BlockObject -> IO ()
renderBlockObjectWireFrames ray bos = do
  let m = rayTraceBlockObjects ray bos
  case m of
    Nothing -> return ()
    Just (ref, removeCoord, placeCoord) -> preservingMatrix $ do
      let blockObject = L.get (item ref) bos
      translate (L.get linPos blockObject)
      rotate (L.get angPos blockObject)
      uscale (L.get blockSize blockObject)

      lighting $= Disabled
      texture Texture2D $= Disabled

      translate (vector3 0.5 0.5 0.5)

      preservingMatrix $ do
        color (vector3 1 0 0)
        translate (fmap fromIntegral removeCoord)
        renderObject Wireframe (Cube 1)

      preservingMatrix $ do
        color (vector3 0 1 0)
        translate (fmap fromIntegral placeCoord)
        renderObject Wireframe (Cube 1)

      lighting $= Enabled
      texture Texture2D $= Enabled

renderBlockObjects :: Map (Ref BlockObject) BlockObject -> IO ()
renderBlockObjects blockObjects = mapM_ renderBlockObject (M.elems blockObjects)

renderBlockObject :: BlockObject -> IO ()
renderBlockObject blockObject = preservingMatrix $ do
  translate (L.get linPos blockObject)
  rotate (L.get angPos blockObject)
  uscale (L.get blockSize blockObject)
  let bs = M.assocs (L.get blocks blockObject)
  mapM_ (uncurry renderBlock) bs

renderBlock :: Vector3 Int -> Block -> IO ()
renderBlock pos block = preservingMatrix $ do
  translate (fmap fromIntegral pos)
  renderPrimitive TriangleStrip $ do
    -- Front
    normal (vector3 0 0 (-1))
    texCoord (vector2 0 0)
    vertex (vector3 1 0 0)
    texCoord (vector2 0 1)
    vertex (vector3 1 1 0)
    texCoord (vector2 1 0)
    vertex (vector3 0 0 0)
    texCoord (vector2 1 1)
    vertex (vector3 0 1 0)
  renderPrimitive TriangleStrip $ do
    -- Top
    normal (vector3 0 1 0)
    texCoord (vector2 0 0)
    vertex (vector3 1 1 0)
    texCoord (vector2 1 0)
    vertex (vector3 0 1 0)
    texCoord (vector2 0 1)
    vertex (vector3 1 1 1)
    texCoord (vector2 1 1)
    vertex (vector3 0 1 1)
  renderPrimitive TriangleStrip $ do
    -- Back
    normal (vector3 0 0 1)
    texCoord (vector2 0 0)
    vertex (vector3 0 1 1)
    texCoord (vector2 1 0)
    vertex (vector3 1 1 1)
    texCoord (vector2 0 1)
    vertex (vector3 0 0 1)
    texCoord (vector2 1 1)
    vertex (vector3 1 0 1)
  renderPrimitive TriangleStrip $ do
    -- Bottom
    normal (vector3 0 (-1) 0)
    texCoord (vector2 0 0)
    vertex (vector3 0 0 0)
    texCoord (vector2 1 0)
    vertex (vector3 1 0 0)
    texCoord (vector2 0 1)
    vertex (vector3 0 0 1)
    texCoord (vector2 1 1)
    vertex (vector3 1 0 1)
  renderPrimitive TriangleStrip $ do
    -- Left
    normal (vector3 1 0 0)
    texCoord (vector2 0 0)
    vertex (vector3 1 0 1)
    texCoord (vector2 1 0)
    vertex (vector3 1 0 0)
    texCoord (vector2 0 1)
    vertex (vector3 1 1 1)
    texCoord (vector2 1 1)
    vertex (vector3 1 1 0)
  renderPrimitive TriangleStrip $ do
    -- Right
    normal (vector3 (-1) 0 0)
    texCoord (vector2 0 0)
    vertex (vector3 0 0 0)
    texCoord (vector2 1 0)
    vertex (vector3 0 0 1)
    texCoord (vector2 0 1)
    vertex (vector3 0 1 0)
    texCoord (vector2 1 1)
    vertex (vector3 0 1 1)

drawAxes :: IO ()
drawAxes = do
  lighting $= Disabled
  texture Texture2D $= Disabled
  renderPrimitive Lines $ do
    color (vector3 1 0 0)
    vertex (vector3 0 0 0)
    vertex (vector3 1 0 0)
  renderPrimitive Lines $ do
    color (vector3 0 1 0)
    vertex (vector3 0 0 0)
    vertex (vector3 0 1 0)
  renderPrimitive Lines $ do
    color (vector3 0 0 1)
    vertex (vector3 0 0 0)
    vertex (vector3 0 0 1)
  lighting $= Enabled
  texture Texture2D $= Enabled

-- OpenGL functions that work with Vectors
convert2 :: (Double -> Double -> f Double) -> Vector2 Double -> f Double
convert2 f (Cons x (Cons y Nil)) = f x y

convert3 :: (Double -> Double -> Double -> f Double) -> Vector3 Double -> f Double
convert3 f (Cons x (Cons y (Cons z Nil))) = f x y z

lookAt :: Vector3 Double -> Vector3 Double -> Vector3 Double -> IO ()
lookAt p df du = GL.lookAt (convert3 GL.Vertex3 p) (convert3 GL.Vertex3 df) (convert3 GL.Vector3 du)

translate :: Vector3 Double -> IO ()
translate = GL.translate . convert3 GL.Vector3

vertex :: Vector3 Double -> IO ()
vertex = GL.vertex . convert3 GL.Vertex3

normal :: Vector3 Double -> IO ()
normal = GL.normal . convert3 GL.Normal3

color :: Vector3 Double -> IO ()
color = GL.color . convert3 GL.Color3

texCoord :: Vector2 Double -> IO ()
texCoord = GL.texCoord . convert2 GL.TexCoord2

scale :: Vector3 Double -> IO ()
scale (Cons x (Cons y (Cons z Nil))) = GL.scale x y z

uscale :: Double -> IO ()
uscale x = GL.scale x x x

rotate :: Vector3 (Vector3 Double) -> IO ()
rotate (Cons (Cons xx (Cons xy (Cons xz Nil))) (Cons (Cons yx (Cons yy (Cons yz Nil))) (Cons (Cons zx (Cons zy (Cons zz Nil))) Nil))) = do
  m <- newMatrix GL.ColumnMajor [
    xx, yx, zx, 0,
    xy, yy, zy, 0,
    xz, yz, zz, 0,
    0, 0, 0, 1]
  multMatrix (m :: GLmatrix GLdouble)

rotateByAngle :: Double -> Vector3 Double -> IO ()
rotateByAngle a = GL.rotate a . convert3 GL.Vector3
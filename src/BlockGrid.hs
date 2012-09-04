module BlockGrid where

import Data.Map
import Control.Monad
import Data.Vector

data Block = NoBlock | TestBlock deriving (Eq, Ord, Show)
--data BlockSide = Top | Bottom | Front | Back | LeftHand | RightHand
type BlockGrid = Map (Vector3 Double) Block

snap :: Vector n Double -> Vector n Double
snap = fmap f where
  f x 
    | x >= 0 = fromIntegral (truncate x)
    | otherwise = fromIntegral (truncate (x - 0.999))

emptyGrid :: BlockGrid
emptyGrid = empty

getBlock :: Vector3 Double -> BlockGrid -> Block
getBlock = findWithDefault NoBlock . snap

setBlock :: Vector3 Double -> Block -> BlockGrid -> BlockGrid
setBlock v NoBlock = delete (snap v)
setBlock v b = insert (snap v) b 

mapMWithKey_ :: Monad m => (k -> a -> m ()) -> Map k a -> m ()
mapMWithKey_ f m = mapM (uncurry f) (toList m) >> return ()
{-# LANGUAGE TemplateHaskell #-}
module Data.BlockObject where

import Prelude hiding (Num (..), repeat)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Algebra
import Data.Vector (Vector3, vector2, vector3, vz, repeat, transpose)
import Data.Label
import Data.Function (on)
import Data.List (sortBy)

import Control.RayTrace
import Data.Body
import Data.LinearAlgebra

data Block = TestBlock deriving (Eq, Ord, Show, Read)

data BlockObject = BlockObject {
  _blockSize :: Double,
  _blocks :: Map (Vector3 Int) Block,
  _blockObjectBody :: Body
} deriving (Show, Read)

$(mkLabels [''BlockObject])

testBlockObject = BlockObject 0.2 (M.fromList [
  (vector3 0 0 0, TestBlock),
  (vector3 0 0 1, TestBlock),
  (vector3 0 0 2, TestBlock)
  ]) (Body 0 startPos zeroComps zeroComps) where
    zeroComps = Components zero zero
    startPos = Components (vector3 1 0 0) (fmap (rotateByAngle (pi Prelude./ 4) (vector3 0 1 0)) one)

instance IsBody BlockObject where
  body = blockObjectBody

--rayTraceBlockObject :: Components -> BlockObject -> Maybe (Vector3 Int, RayTraceResult Vector3 Double)
rayTraceBlockObject ray blockObject = rayTraceBlocks ray' (get blocks blockObject) where
  ray' = relateTo blockObject ray

rayTraceBlocks :: Components -> Map (Vector3 Int) Block -> Maybe (Vector3 Int, RayTraceResult Vector3 Double)
rayTraceBlocks (Components lin ang) bs = if null sortedHits then Nothing else Just (head sortedHits) where
  aabbs = M.mapWithKey (\k _ -> let k' = fmap fromIntegral k in vector2 k' (k' + repeat one)) bs
  rays = M.map (rayTraceAABB lin (vz ang)) aabbs
  hitsOnly = M.filter isHit rays
  sortedHits = sortBy (compare `on` \(_,Hit (dist, _) _) -> dist) (M.assocs hitsOnly)

relateTo :: IsBody a => a -> Components -> Components
relateTo bo (Components lin1 ang1) = Components lin1' ang1' where
  Components lin2 ang2 = get position bo
  lin1' = rotate (negate ang2) . translate (negate lin2) $ lin1
  ang1' = fmap (rotate (negate ang2)) ang1
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
import Data.Maybe (fromJust, isJust)

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
    startPos = Components (vector3 3 2 1) (fmap (rotateByAngle (pi Prelude./ 8) (vector3 0 0 1)) one)

instance IsBody BlockObject where
  body = blockObjectBody

rayTraceBlockObjects :: Ord k => Components -> Map k BlockObject -> Maybe (k, Vector3 Int, Vector3 Int)
rayTraceBlockObjects ray m = fmap convert . safeHead $ sortedResults where
  results = M.map fromJust . M.filter isJust . M.map (rayTraceBlockObject ray) $ m
  sortedResults = sortBy (compare `on` \(_, (dist,_,_)) -> dist) (M.assocs results)
  convert (k, (_, removeCoord, addCoord)) = (k, removeCoord, addCoord)

rayTraceBlockObject :: Components -> BlockObject -> Maybe (Double, Vector3 Int, Vector3 Int)
rayTraceBlockObject ray blockObject = fmap convert result where
  ray' = relateTo blockObject ray
  result = rayTraceBlocks ray' (get blocks blockObject)
  convert (coord, Hit (dist, normal) _) = (dist, coord, coord + fmap round normal)

rayTraceBlocks :: Components -> Map (Vector3 Int) Block -> Maybe (Vector3 Int, RayTraceResult Vector3 Double)
rayTraceBlocks (Components lin ang) bs = safeHead sortedHits where
  aabbs = M.mapWithKey (\k _ -> let k' = fmap fromIntegral k in vector2 k' (k' + repeat one)) bs
  rays = M.map (rayTraceAABB lin (vz ang)) aabbs
  hitsOnly = M.filter isHit rays
  sortedHits = sortBy (compare `on` \(_,Hit (dist, _) _) -> dist) (M.assocs hitsOnly)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

relateTo :: BlockObject -> Components -> Components
relateTo bo (Components lin1 ang1) = Components lin1' ang1' where
  Components lin2 ang2 = get position bo
  size = get blockSize bo
  lin1' = uscale (1 Prelude./ size) . rotate ang2 . translate (negate lin2) $ lin1
  ang1' = fmap (rotate ang2) ang1
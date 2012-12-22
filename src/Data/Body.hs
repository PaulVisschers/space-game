{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Data.Body where

import Prelude hiding (id, (.), Num (..))

import Control.Category
import Data.Label

import Data.Vector
import Data.Algebra
import Data.LinearAlgebra

data Body = Body {
  _bodyMass :: Double,
  _bodyPosition :: Components,
  _bodyVelocity :: Components,
  _bodyAcceleration :: Components
  } deriving (Show, Read)

data Components = Components {
  _linear :: Vector3 Double,
  _angular :: Vector3 (Vector3 Double)
  } deriving (Show, Read)

$(mkLabels [''Body, ''Components])

class IsBody a where
  body :: a :-> Body

mass :: IsBody a => a :-> Double
mass = bodyMass . body

position :: IsBody a => a :-> Components
position = bodyPosition . body

velocity :: IsBody a => a :-> Components
velocity = bodyVelocity . body

acceleration :: IsBody a => a :-> Components
acceleration = bodyAcceleration . body

linPos :: IsBody a => a :-> Vector3 Double
linPos = linear . position

angPos :: IsBody a => a :-> Vector3 (Vector3 Double)
angPos = angular . position

linVel :: IsBody a => a :-> Vector3 Double
linVel = linear . velocity

angVel :: IsBody a => a :-> Vector3 (Vector3 Double)
angVel = angular . velocity

linAcc :: IsBody a => a :-> Vector3 Double
linAcc = linear . acceleration

angAcc :: IsBody a => a :-> Vector3 (Vector3 Double)
angAcc = angular . acceleration

updateBody :: IsBody a => Double -> a -> a
updateBody dt x = set linPos lp . set linVel lv . set angPos ap . set angVel av $ x where
  lp = get linPos x + dt *> lv
  lv = get linVel x + dt *> get linAcc x
  ap = fmap (rotate av) (get angPos x)
  av = fmap (rotate (get angAcc x)) (get angVel x)

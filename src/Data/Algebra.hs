{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Data.Algebra where

import Data.Foldable (Foldable, foldr)
import GHC.Int (Int32)
import Prelude (Double, Float, Int, Integer)

import qualified Prelude as P

infixl 6 +, -
infixl 7 *, /

class Group a where
  zero :: a
  negate :: a -> a
  (+), (-) :: a -> a -> a

  negate x = zero - x
  x - y = x + negate y

sum :: (Foldable f, Group a) => f a -> a
sum = foldr (+) zero

class Multiplicative a b where
  type Multiplied a b
  (*) :: a -> b -> Multiplied a b

class (Group a, Multiplicative a a, Multiplied a a ~ a) => Ring a where
  one :: a

(*.) :: (Multiplicative a a, Multiplied a a ~ a) => a -> a -> a
(*.) = (*)

product :: (Foldable f, Ring a) => f a -> a
product = foldr (*) one

class Ring a => DivisionRing a where
  recip :: a -> a
  (/) :: a -> a -> a

  recip x = one / x
  x / y = x * recip y

class DivisionRing a => Real a where
  pi :: a
  exp :: a -> a
  sqrt :: a -> a
  (^) :: a -> a -> a
  log :: a -> a -> a
  sin :: a -> a
  tan :: a -> a
  cos :: a -> a
  asin :: a -> a
  atan :: a -> a
  acos :: a -> a
  sinh :: a -> a
  tanh :: a -> a
  cosh :: a -> a
  asinh :: a -> a
  atanh :: a -> a
  acosh :: a -> a

-- Instances for common numeric types
instance Group Int where
  zero = 0
  (+) = (P.+)
  (-) = (P.-)
  negate = P.negate

instance Multiplicative Int Int where
  type Multiplied Int Int = Int
  (*) = (P.*)

instance Ring Int where
  one = 1

instance Group Integer where
  zero = 0
  (+) = (P.+)
  (-) = (P.-)
  negate = P.negate

instance Multiplicative Integer Integer where
  type Multiplied Integer Integer = Integer
  (*) = (P.*)

instance Ring Integer where
  one = 1

instance Group Int32 where
  zero = 0
  (+) = (P.+)
  (-) = (P.-)
  negate = P.negate

instance Multiplicative Int32 Int32 where
  type Multiplied Int32 Int32 = Int32
  (*) = (P.*)

instance Ring Int32 where
  one = 1

instance Group Float where
  zero = 0
  (+) = (P.+)
  (-) = (P.-)
  negate = P.negate

instance Multiplicative Float Float where
  type Multiplied Float Float = Float
  (*) = (P.*)

instance Ring Float where
  one = 1

instance DivisionRing Float where
  recip = P.recip
  (/) = (P./)

instance Real Float where
  pi = P.pi
  exp = P.exp
  sqrt = P.sqrt
  (^) = (P.**)
  log = P.logBase
  sin = P.sin
  tan = P.tan
  cos = P.cos
  asin = P.asin
  atan = P.atan
  acos = P.acos
  sinh = P.sinh
  tanh = P.tanh
  cosh = P.cosh
  asinh = P.asinh
  atanh = P.atanh
  acosh = P.acosh

instance Group Double where
  zero = 0
  (+) = (P.+)
  (-) = (P.-)
  negate = P.negate

instance Multiplicative Double Double where
  type Multiplied Double Double = Double
  (*) = (P.*)

instance Ring Double where
  one = 1

instance DivisionRing Double where
  recip = P.recip
  (/) = (P./)

instance Real Double where
  pi = P.pi
  exp = P.exp
  sqrt = P.sqrt
  (^) = (P.**)
  log = P.logBase
  sin = P.sin
  tan = P.tan
  cos = P.cos
  asin = P.asin
  atan = P.atan
  acos = P.acos
  sinh = P.sinh
  tanh = P.tanh
  cosh = P.cosh
  asinh = P.asinh
  atanh = P.atanh
  acosh = P.acosh
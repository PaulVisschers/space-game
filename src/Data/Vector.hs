{-# LANGUAGE DataKinds, GADTs, FlexibleContexts, FlexibleInstances, KindSignatures, TypeFamilies, TypeOperators #-}
module Data.Vector where

import Control.Applicative(Applicative, (<$>), (<*>), pure)
import Data.Foldable (Foldable, foldr)
import Data.Traversable(Traversable, traverse)
import Prelude (Bool(True), Eq, Functor, Ord, Ordering(EQ), (==), (&&), (>), ($), (.), compare, fmap, id, return)
import Text.Read (Read, Lexeme(Ident), lexP, parens, prec, readPrec, step)
import Text.Show (Show, showParen, showsPrec, showString)

-- * Type-level natural numbers
data Nat = Zero | Succ Nat

type family (:+) (a :: Nat) (b :: Nat) :: Nat
type instance Zero :+ y = y
type instance Succ x :+ y = Succ (x :+ y)

data Natural n where
  NZero :: Natural Zero
  NSucc :: Natural n -> Natural (Succ n)

class IsNat n where
  unit :: Natural n

instance IsNat Zero where
  unit = NZero

instance IsNat n => IsNat (Succ n) where
  unit = NSucc unit

-- * Vector type
data Vector :: Nat -> * -> * where
  Nil :: Vector Zero a
  Cons :: a -> Vector n a -> Vector (Succ n) a

instance Eq a => Eq (Vector n a) where
  Nil == Nil = True
  Cons x xs == Cons y ys = x == y && xs == ys

instance Ord a => Ord (Vector n a) where
  compare Nil Nil = EQ
  compare (Cons x xs) (Cons y ys) = if ord == EQ then compare xs ys else ord where
    ord = compare x y

instance Show (Vector Zero a) where
  showsPrec d Nil = showParen (d > 11) $ showString "Nil"

instance (Show a, Show (Vector n a)) => Show (Vector (Succ n) a) where
  showsPrec d (Cons x xs) = showParen (d > 10) $ showString "Cons " . showsPrec 11 x . showString " " . showsPrec 11 xs

instance Read (Vector Zero a) where
  readPrec = parens $ (prec 11 $ do
    Ident "Nil" <- lexP
    return Nil)

instance (Read a, Read (Vector n a)) => Read (Vector (Succ n) a) where
  readPrec = parens (prec 10 $ do
    Ident "Cons" <- lexP
    x <- step readPrec
    xs <- step readPrec
    return (Cons x xs))

instance Functor (Vector n) where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance IsNat n => Applicative (Vector n) where
  pure = repeat
  (<*>) = apply

instance Foldable (Vector n) where
  foldr _ z Nil = z
  foldr f z (Cons x xs) = f x (foldr f z xs)

instance Traversable (Vector n) where
  traverse f Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

snoc :: a -> Vector n a -> Vector (Succ n) a
snoc x Nil = Cons x Nil
snoc x (Cons y v) = Cons y (snoc x v)

head :: Vector (Succ n) a -> a
head (Cons x _) = x

tail :: Vector (Succ n) a -> Vector n a
tail (Cons _ xs) = xs

last :: Vector (Succ n) a -> a
last (Cons x Nil) = x
last (Cons _ (Cons x v)) = last (Cons x v)

init :: Vector (Succ n) a -> Vector n a
init (Cons _ Nil) = Nil
init (Cons x (Cons y v)) = Cons x (init (Cons y v))

(++) :: Vector n a -> Vector m a -> Vector (n :+ m) a
Nil ++ ys = ys
(Cons x xs) ++ ys = Cons x (xs ++ ys)

unfoldr :: IsNat n => (b -> (a, b)) -> b -> Vector n a
unfoldr = unfoldr' unit where
  unfoldr' :: Natural n -> (b -> (a, b)) -> b -> Vector n a
  unfoldr' NZero _ _ = Nil
  unfoldr' (NSucc n) f z = let (x, z') = f z in Cons x (unfoldr' n f z')

repeat :: IsNat n => a -> Vector n a
repeat = unfoldr (\x -> (x,x))

iterate :: IsNat n => (a -> a) -> a -> Vector n a
iterate f = unfoldr (\x -> (x, f x))

apply :: Vector n (a -> b) -> Vector n a -> Vector n b
apply Nil Nil = Nil
apply (Cons f fs) (Cons x xs) = Cons (f x) (apply fs xs)

zipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith f x1 x2 = fmap f x1 `apply` x2

zipWith3 :: (a -> b -> c -> d) -> Vector n a -> Vector n b -> Vector n c -> Vector n d
zipWith3 f x1 x2 x3 = zipWith f x1 x2 `apply` x3

zipWith4 :: (a -> b -> c -> d -> e) -> Vector n a -> Vector n b -> Vector n c -> Vector n d -> Vector n e
zipWith4 f x1 x2 x3 x4 = zipWith3 f x1 x2 x3 `apply` x4

zipWith5 :: (a -> b -> c -> d -> e -> f) -> Vector n a -> Vector n b -> Vector n c -> Vector n d -> Vector n e -> Vector n f
zipWith5 f x1 x2 x3 x4 x5 = zipWith4 f x1 x2 x3 x4 `apply` x5

type Matrix n m a = Vector n (Vector m a)

transpose :: IsNat m => Matrix n m a -> Matrix m n a
transpose = traverse id

-- ** Short-hand constructors and selectors
type Vector1 = Vector (Succ Zero)
type Vector2 = Vector (Succ (Succ Zero))
type Vector3 = Vector (Succ (Succ (Succ Zero)))
type Vector4 = Vector (Succ (Succ (Succ (Succ Zero))))

vx :: Vector n a -> a
vx (Cons x _) = x

vy :: Vector (Succ n) a -> a
vy (Cons _ (Cons y _)) = y

vz :: Vector (Succ (Succ n)) a -> a
vz (Cons _ (Cons _ (Cons z _))) = z

vector1 :: a -> Vector1 a
vector1 x = Cons x Nil

vector2 :: a -> a  -> Vector2 a
vector2 x y = Cons x (Cons y Nil)

vector3 :: a -> a -> a -> Vector3 a
vector3 x y z = Cons x (Cons y (Cons z Nil))

vector4 :: a -> a -> a -> a -> Vector4 a
vector4 x y z w = Cons x (Cons y (Cons z (Cons w Nil)))

--zip :: Vector n a -> Vector n b -> Vector n (a,b)
--zip = zipWith (,)

--zipWith4 :: (a -> b -> c -> d -> e) -> Vector n a -> Vector n b -> Vector n c -> Vector n d -> Vector n e
--zipWith4 _ Nil Nil Nil Nil = Nil
--zipWith4 f (Cons x xs) (Cons y ys) (Cons z zs) (Cons w ws) = Cons (f x y z w) (zipWith4 f xs ys zs ws)

--zipWith5 :: (a -> b -> c -> d -> e -> f) -> Vector n a -> Vector n b -> Vector n c -> Vector n d -> Vector n e -> Vector n f
--zipWith5 _ Nil Nil Nil Nil Nil = Nil
--zipWith5 f (Cons x xs) (Cons y ys) (Cons z zs) (Cons w ws) (Cons v vs) = Cons (f x y z w v) (zipWith5 f xs ys zs ws vs)

--unzip :: Vector n (a, b) -> (Vector n a, Vector n b)
--unzip Nil = (Nil, Nil)
--unzip (Cons (x, y) xys) = (Cons x xs, Cons y ys) where 
--  (xs, ys) = unzip xys
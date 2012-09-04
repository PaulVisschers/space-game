{-# LANGUAGE GADTs, EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}
module Data.Record where

data RNil
data RCons l a s

data Record s where
  RNil :: Record RNil
  RCons :: a -> Record s -> Record (RCons l a s)

instance Eq (Record RNil) where
  RNil == RNil = True

instance (Eq a, Eq (Record s)) => Eq (Record (RCons l a s)) where
  RCons x xs == RCons y ys = x == y && xs == ys

instance Ord (Record RNil) where
  compare RNil RNil = EQ

instance (Ord a, Ord (Record s)) => Ord (Record (RCons l a s)) where
  compare (RCons x xs) (RCons y ys) = case compare x y of
    EQ -> compare xs ys
    c -> c

instance Show (Record RNil) where
  showsPrec n RNil = showString "RNil"

instance (Show a, Show (Record s)) => Show (Record (RCons l a s)) where
  showsPrec n (RCons x xs) = showParen (n > 10) $ showString "RCons " . showsPrec 11 x . showString " " . showsPrec 11 xs

class HasField l s a | l s -> a where
  getField :: l -> Record s -> a
  putField :: l -> a -> Record s -> Record s
  modifyField :: l -> (a -> a) -> Record s -> Record s

instance HasField l (RCons l a s) a where
  getField l (RCons x xs) = x
  putField l x (RCons y ys) = RCons x ys
  modifyField l f (RCons x xs) = RCons (f x) xs

instance HasField l1 s a => HasField l1 (RCons l2 b s) a where
  getField l (RCons x xs) = getField l xs
  putField l x (RCons y ys) = RCons y (putField l x ys)
  modifyField l f (RCons x xs) = RCons x (modifyField l f xs)
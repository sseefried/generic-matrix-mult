{-# LANGUAGE TypeOperators, DeriveTraversable, DeriveFunctor, DeriveFoldable, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances, CPP, GADTs, EmptyDataDecls, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
--
-- The most minimal file for generic
-- matrix multiplication.
--
module MinGenMatMult where

-- standard imports
import Prelude hiding (replicate, foldl, sum)
import Data.Traversable
import Control.Applicative
import Data.Foldable hiding (toList, sum)
import Data.Monoid
import Text.Printf

dotGen :: (Foldable f, Applicative f, Monoid p, Monoid s)
       => (a -> p, p -> a) -> (a -> s, s-> a) -> f a -> f a -> a
dotGen (pinject, pproject) (sinject, sproject) x y =
   sproject . fold . fmap (sinject . pproject) $ liftA2 mappend px py
  where
    px = fmap pinject x
    py = fmap pinject y

--
-- Generalised dot products. Works on Vec, Pair, Tree (and much, much more!)
--
dot :: (Num a, Foldable f, Applicative f) => f a -> f a -> a
dot = dotGen (Product, getProduct) (Sum, getSum)

transpose :: (Traversable f1, Applicative f2) => f1 (f2 a) -> f2 (f1 a)
transpose = sequenceA

mmult :: (Num a, Applicative f1, Applicative f2, Applicative f3, Traversable f1, Traversable f2)
       => f1 (f2 a) -> f2 (f3 a) -> f1 (f3 a)
mmult m1 m2 = fmap (flip (fmap . dot) (transpose m2)) m1

class Applicative f => Identity f where
  identity :: Num a => f (f a)

-----------------------------------
--

--
-- Unit
--
data Unit a = Unit deriving (Show, Functor, Foldable, Traversable)

--
-- Id
--
newtype Id a = Id a deriving (Show, Functor, Foldable, Traversable)

--
-- Products
--
data (f :*: g) a = f a :*: g a             deriving (Show, Functor, Traversable)


-- Sum Types not needed
-- --------------------
-- We don't need the :+: constructor because we write n instances for each "shaped" type
-- e.g.  For Vectors
--
-- instance EncodeF (Vec Z) where
--   type Enc (Vec Z) = Unit
--   encode Nil  = Unit
--   decode Unit = Nil
--
-- instance Nat n => EncodeF (Vec (S n)) where
--   type Enc (Vec (S n)) = (Id :*: Vec n)
--   encode (Cons x xs) = Id x :*: xs
--   decode = aux
--     where
--       aux :: (Id :*: Vec n) a -> Vec (S n) a
--       aux (Id x :*: xs) = Cons x xs
--
-- This is interesting because, how do you write an Applicative instance anyway?
--
--  instance (Applicative f, Applicative g) => Applicative (f :+: g) where
--    pure a = ??? -- Is it 'InL a' or 'InR a'?
--


-- For some reason GHC does not derive the correct Foldable instance.
instance (Foldable f, Foldable g) => Foldable (f :*: g) where
  foldMap f (a :*: b) = foldMap f a `mappend` foldMap f b

instance Applicative Unit where
   pure _ = Unit
   Unit <*> Unit = Unit

instance Applicative Id where
   pure a = Id a
   (Id fa) <*> (Id a) = Id (fa a)

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
  pure a                  = (pure a)  :*: (pure a)
  (f :*: g) <*> (a :*: b) = (f <*> a) :*: (g <*> b)

instance (Identity f, Identity g) => Identity (f :*: g) where
  identity = fmap (:*: pure 0) identity :*: fmap (pure 0 :*:) identity

instance Identity Unit where
  identity = Unit

instance Identity Id where
  identity = Id (Id 1)

-------------

class EncodeF f where
  type Enc f :: * -> *
  encode :: f a -> Enc f a
  decode :: Enc f a -> f a

------------------------------------------------------------
-- Examples

data S n
data Z

data Vec n a where
  Nil  :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a

class Nat n where
  natToInt :: n -> Int

instance Nat Z where
  natToInt _   = 0

instance Nat n => Nat (S n) where
  natToInt (_::S n) = 1 + natToInt (undefined :: n)

toList :: Vec n a  -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

instance (Nat n, Show a) => Show (Vec n a) where
  show vec = printf "<%s|%s>" (show . toList $ vec) (show . natToInt $ (undefined :: n))

--------------------
-- EncodeF instances

#define TYPE Vec
#include "instances-inc.hs"

instance EncodeF (Vec Z) where
  type Enc (Vec Z) = Unit
  encode Nil  = Unit
  decode Unit = Nil

instance Nat n => EncodeF (Vec (S n)) where
  type Enc (Vec (S n)) = (Id :*: Vec n)
  encode (Cons x xs) = Id x :*: xs
  decode = aux
    where
      aux :: (Id :*: Vec n) a -> Vec (S n) a
      aux (Id x :*: xs) = Cons x xs
--------


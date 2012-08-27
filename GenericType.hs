{-# LANGUAGE TypeOperators, DeriveFoldable, DeriveTraversable, DeriveFunctor, TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
--
-- Author: Sean Seefried
--
-- | This module defines generic types and Functor, Foldable and Traversable instances for each.
--   Generic type are built out of unit, singleton, sums and products.
--   We don't include sums.
module GenericType where

-- standard libraries
import Control.Applicative
import Data.Foldable
import Data.Traversable

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
data (f :*: g) a = f a :*: g a             deriving (Show, Functor, Traversable, Foldable)


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
--instance (Foldable f, Foldable g) => Foldable (f :*: g) where
--  foldMap f (a :*: b) = foldMap f a `mappend` foldMap f b

instance Applicative Unit where
   pure _ = Unit
   Unit <*> Unit = Unit

instance Applicative Id where
   pure a = Id a
   (Id fa) <*> (Id a) = Id (fa a)

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
  pure a                  = (pure a)  :*: (pure a)
  (f :*: g) <*> (a :*: b) = (f <*> a) :*: (g <*> b)

--------------------

class EncodeF f where
  type Enc f :: * -> *
  encode :: f a -> Enc f a
  decode :: Enc f a -> f a
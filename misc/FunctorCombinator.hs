{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable, ScopedTypeVariables #-}
module FunctorCombinator where

import Prelude hiding (concat, sequence, zip, unzip, replicate)
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.Applicative
import Control.Arrow
import Text.Printf

data Pair a = a :# a deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Pair where
  pure a                  = a :# a
  (fa :# fb) <*> (a :# b) = fa a :# fb b

data Unit a = Unit deriving (Show, Functor, Foldable, Traversable)

instance Applicative Unit where
   pure _ = Unit
   Unit <*> Unit = Unit

newtype Id a = Id a deriving (Show, Functor, Foldable, Traversable)

instance Applicative Id where
   pure a = Id a
   (Id fa) <*> (Id a) = Id (fa a)

-- Product types
data (f :*: g) a = f a :*: g a             deriving (Show, Functor, Traversable)

-- For some reason GHC does not derive the correct Foldable instance.
instance (Foldable f, Foldable g) => Foldable (f :*: g) where
  fold (a :*: b) = fold a `mappend` fold b

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
  pure a                  = (pure a)  :*: (pure a)
  (f :*: g) <*> (a :*: b) = (f <*> a) :*: (g <*> b)

--
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

-- Type composition
newtype (g :.: f) a = O { unO :: g (f a) } deriving (Show, Traversable)

instance (Functor f, Functor g) => Functor (g :.: f) where
  fmap h (O gfa) = O . fmap (fmap h) $ gfa

instance (Foldable f, Functor g, Foldable g) => Foldable (g :.: f) where
  fold (O gfm) = fold . (fmap fold) $ gfm

instance (Applicative f, Applicative g) => Applicative (g :.: f) where
  pure                 = O . pure . pure
  (O gfab) <*> (O gfa) = O $ (<*>) <$> gfab <*> gfa

class EncodeF f where
  type Enc f :: * -> *
  encode :: f a -> Enc f a
  decode :: Enc f a -> f a
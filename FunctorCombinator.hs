{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveFunctor, DeriveFoldable #-} 
{-# LANGUAGE DeriveTraversable #-}
module FunctorCombinator where
  
import Prelude hiding (concat, sequence, zip, unzip, replicate)
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.Applicative
import Control.Arrow 
import Text.Printf

data Pair a = a :# a deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

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
newtype (g :.: f) a = O { unO :: g (f a) } deriving (Show, Functor, Foldable, Traversable)

class Replicate f where
   replicate :: a -> f a
   
instance Replicate Unit where
   replicate _ = Unit
   
instance Replicate Id where
  replicate a = Id a

instance (Replicate f, Replicate g) => Replicate (f :*: g) where
  replicate a = replicate a :*: replicate a

class EncodeF f where
  type Enc f :: * -> * 
  encode :: f a -> Enc f a
  decode :: Enc f a -> f a
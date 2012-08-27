{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveFunctor, DeriveFoldable #-} 
{-# LANGUAGE DeriveTraversable, GADTs, EmptyDataDecls, FlexibleInstances, ScopedTypeVariables #-}
module FunctorCombinatorsWithShape where

--
-- I didn't end up using this but it's interesting anyway.
--

import Prelude hiding (concat, sequence, zip, unzip, replicate)
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.Applicative
import Control.Arrow 
import Text.Printf

data L :: * -> *
data R :: * -> *

data Unit sh a where
  Unit :: Unit () a

data Id sh a where
  Id :: a -> Id () a
  
data (f :+: g) sh a where
  InL :: f sh a -> (f :+: g) (L sh) a
  InR :: g sh a -> (f :+: g) (R sh) a

instance Show a => Show (Unit sh a) where
  show Unit = "Unit"

instance Show a => Show (Id sh a) where
  show (Id a) = printf "Id %s" (show a)
  
instance Show (f sh a) => Show ((f :+: g) (L sh) a) where
  show (InL a) = printf "InL (%s)" (show a)

instance Show (g sh a) => Show ((f :+: g) (R sh) a) where
  show (InR a) = printf "InR (%s)" (show a)

instance (Show (f sh a), Show (g sh' a)) => Show ((f :*: g) (sh, sh') a) where
  show (fa :*: ga) = printf "(%s) :*: (%s)" (show fa) (show ga)

data (f :*: g) sh a where
  (:*:) :: f sh a -> g sh' a -> (f :*: g) (sh, sh') a
  
class Replicate f where
  replicate :: a -> f a

instance Replicate (Unit ()) where
  replicate _ = Unit
  
instance Replicate (Id ()) where
  replicate a = Id a
  
instance Replicate (f sh) => Replicate ((f :+: g) (L sh)) where
  replicate a = InL (replicate a)
  
instance Replicate (g sh) => Replicate ((f :+: g) (R sh)) where
  replicate a = InR (replicate a)
  
instance (Replicate (f sh), Replicate (g sh')) => Replicate ((f :*: g) (sh, sh')) where
  replicate a = replicate a :*: replicate a
  
instance Functor (Unit ()) where
  fmap _ Unit = Unit

instance Foldable (Unit ()) where
  fold _ = mempty

instance Applicative (Unit ()) where
  pure _ = Unit
  Unit <*> Unit = Unit
  
instance Functor (Id ()) where
  fmap f (Id a) = Id (f a)

instance Foldable (Id ()) where
  fold (Id a) = a

instance Applicative (Id ()) where
  pure a             = Id a
  (Id fa) <*> (Id a) = Id (fa a)
  
instance Functor (f sh) => Functor ((f :+: g) (L sh)) where
  fmap f (InL a) = InL (fmap f a)
  
instance Foldable (f sh) => Foldable ((f :+: g) (L sh)) where
  fold (InL a) = fold a

instance Applicative (f sh) => Applicative ((f :+: g) (L sh)) where
  pure a               = InL (pure a)
  (InL fa) <*> (InL a) = InL (fa <*> a)

instance Functor (g sh) => Functor ((f :+: g) (R sh)) where
  fmap f (InR a) = InR (fmap f a)

instance Foldable (g sh) => Foldable ((f :+: g) (R sh)) where
  fold (InR a) = fold a

instance Applicative (g sh) => Applicative ((f :+: g) (R sh)) where
  pure a               = InR (pure a)
  (InR fa) <*> (InR a) = InR (fa <*> a)

instance (Functor (f sh), Functor (g sh')) => Functor ((f :*: g) (sh, sh')) where
  fmap f (a :*: b) = (fmap f a :*: fmap f b)

instance (Foldable (f sh), Foldable (g sh')) => Foldable ((f :*: g) (sh, sh')) where
  fold (a :*: b) = fold a `mappend` fold b

instance (Applicative (f sh), Applicative (g sh')) => Applicative ((f :*: g) (sh, sh')) where
  pure a = (pure a :*: pure a)
  (fa :*: fb) <*> (a :*: b) = ((fa <*> a) :*: (fb <*> b))

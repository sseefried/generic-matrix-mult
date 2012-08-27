{-# LANGUAGE ScopedTypeVariables, GADTs, EmptyDataDecls, CPP, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies, UndecidableInstances #-}

module Tree where

import Text.Printf
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Data.Monoid

--
-- Trees
--

--
-- We specify the shape of a tree using nested-pairs of type unit (i.e. '()').
-- This is to ensure that we are taking the dot product of two things of the same structure.
-- It also helps ensures that the Applicative instance for 'Tree's contains total functions.
--

-- sh for shape
data Tree sh a where
  Leaf   :: a -> Tree () a
  Branch :: Tree m a -> Tree n a -> Tree (m,n) a

instance Show a => Show (Tree sh a) where
  show (Leaf a) = show a
  show (Branch s t) = printf "{%s,%s}" (show s) (show t)

{--}

instance Functor (Tree sh) where
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Branch s t) = Branch (fmap f s) (fmap f t)

instance Foldable (Tree sh) where
  -- (a -> m) -> Tree n a -> m
  foldMap f (Leaf a)     = f a
  foldMap f (Branch s t) = foldMap f s `mappend` foldMap f t

--
-- Trees have to have the same shape.
--
instance Applicative (Tree ()) where
  pure a                          = Leaf a
  Leaf fa <*> Leaf a              = Leaf (fa a)

instance (Applicative (Tree m), Applicative (Tree n)) => Applicative (Tree (m,n)) where
  pure a                          = Branch (pure a) (pure a)
  (Branch fs ft) <*> (Branch s t) = Branch (fs <*> s) (ft <*> t)

instance Traversable (Tree sh) where
  traverse f (Leaf a)     = Leaf   <$> f a
  traverse f (Branch s t) = Branch <$> traverse f s <*> traverse f t
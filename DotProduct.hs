{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, DeriveFunctor, DeriveFoldable, TypeFamilies #-}
module DotProduct where

import Data.Traversable
import Control.Applicative
import Data.Foldable hiding (toList)
import Data.Monoid
import Text.Printf

data S n
data Z

--
-- Vecs with length encoded. Prevents us from taking the dot product of two lists of
-- unequal length.
--
-- It also helps ensure that the Applicative instance for 'Vec's contains total functions.
--
data Vec n a where
  Nil  :: Vec Z a
  Cons :: Nat n => a -> Vec n a -> Vec (S n) a

class Nat n where
  replicateL :: a -> Vec n a

instance Nat Z where
  replicateL _ = Nil

instance Nat n => Nat (S n) where
  replicateL a = a `Cons` replicateL a

toList :: Vec n a  -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

instance Show a => Show (Vec n a) where
  show = show . toList

infixr 5 `Cons`

type One   = S Z
type Two   = S One
type Three = S Two

instance Nat n => Foldable (Vec n) where
  -- foldMap :: Monoid m => (a -> m) -> Vec n a -> m
  foldMap f Nil         = mempty
  foldMap f (Cons x xs) = f x `mappend` foldMap f xs

instance Functor (Vec n) where
  fmap f Nil           = Nil
  fmap f (x `Cons` xs) = f x `Cons` fmap f xs

--
-- Vecs have to have the same length
--
instance Nat n => Applicative (Vec n) where
  pure x   = replicateL x
  Nil <*> Nil                       = Nil
  (fa `Cons` fas) <*> (a `Cons` as) = fa a `Cons` (fas <*> as)

instance Nat n => Traversable (Vec n) where
  traverse _ Nil           = pure Nil
  traverse f (x `Cons` xs) = Cons <$> f x <*> traverse f xs

list1 :: Vec Two Integer
list1 = 1 `Cons` 2 `Cons` Nil

list2 :: Vec Two Integer
list2 = 3 `Cons` 4 `Cons` Nil

--
-- Pairs
--

data Pair a = a :# a deriving (Functor, Foldable)

instance Applicative Pair where
  pure x = x :# x
  (fa :# fb) <*> (a :# b) = fa a :# fb b

--
-- Trees
--

--
-- We specify the shape of a tree using nested-pairs of type unit (i.e. '()').
-- This is to ensure that we are taking the dot product of two things of the same structure.
-- It also helps ensures that the Applicative instance for 'Tree's contains total functions.
--
class Shape sh where
  replicateT :: a -> Tree sh a

instance Shape () where
  replicateT a = Leaf a

instance (Shape sh1, Shape sh2) => Shape (sh1, sh2) where
  replicateT a = Branch (replicateT a) (replicateT a)

-- sh for shape
data Tree sh a where
  Leaf   :: a -> Tree () a
  Branch :: (Shape m, Shape n) => Tree m a -> Tree n a -> Tree (m,n) a

instance Show a => Show (Tree sh a) where
  show (Leaf a) = show a
  show (Branch s t) = printf "{%s,%s}" (show s) (show t)

instance Shape sh => Functor (Tree sh) where
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Branch s t) = Branch (fmap f s) (fmap f t)

instance Shape sh => Foldable (Tree sh) where
  -- (a -> m) -> Tree n a -> m
  foldMap f (Leaf a)     = f a
  foldMap f (Branch s t) = foldMap f s `mappend` foldMap f t

--
-- Trees have to have the same shape.
--
instance Shape sh => Applicative (Tree sh) where
  pure a                          = replicateT a
  Leaf fa <*> Leaf a              = Leaf (fa a)
  (Branch fs ft) <*> (Branch s t) = Branch (fs <*> s) (ft <*> t)

instance Shape sh => Traversable (Tree sh) where
  traverse f (Leaf a)     = Leaf <$> f a
  traverse f (Branch s t) = Branch <$> traverse f s <*> traverse f t

tree1 :: Tree ((), ((), ())) Integer
tree1 = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))

tree2 :: Tree ((), ((), ())) Integer
tree2 = Branch (Leaf 4) (Branch (Leaf 5) (Leaf 6))

--
-- Generalised dot products. Works on Vec, Pair, Tree (and much, much more!)
--
dot :: (Num a, Foldable f, Applicative f) => f a -> f a -> a
dot x y = (getSum . fold . fmap (Sum . getProduct)) (liftA2 mappend px py)
  where
    px = fmap Product x
    py = fmap Product y
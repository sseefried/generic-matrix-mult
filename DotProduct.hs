{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE CPP, TypeOperators, TypeFamilies #-}
module DotProduct where

import Prelude hiding (replicate, foldl)
import Data.Traversable
import Control.Applicative
import Data.Foldable hiding (toList)
import Data.Monoid
import Text.Printf

--friends
import FunctorCombinator

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
  natToInt :: n -> Int

instance Nat Z where
  replicateL _ = Nil
  natToInt _   = 0

instance Nat n => Nat (S n) where
  replicateL a = a `Cons` replicateL a
  natToInt (_::S n) = 1 + natToInt (undefined :: n)

toList :: Vec n a  -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

instance (Nat n, Show a) => Show (Vec n a) where
  show vec = printf "<%s|%s>" (show . toList $ vec) (show . natToInt $ (undefined :: n))

infixr 5 `Cons`

type One   = S Z
type Two   = S One
type Three = S Two

{--
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
--}

{--}

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

--}


vec1 :: Vec Two Integer
vec1 = 1 `Cons` 2 `Cons` Nil

vec2 :: Vec Two Integer
vec2 = 3 `Cons` 4 `Cons` Nil

--
-- Trees
--

--
-- We specify the shape of a tree using nested-pairs of type unit (i.e. '()').
-- This is to ensure that we are taking the dot product of two things of the same structure.
-- It also helps ensures that the Applicative instance for 'Tree's contains total functions.
--
class TreeShape sh where
  replicateT :: a -> Tree sh a

instance TreeShape () where
  replicateT a = Leaf a

instance (TreeShape sh1, TreeShape sh2) => TreeShape (sh1, sh2) where
  replicateT a = Branch (replicateT a) (replicateT a)

-- sh for shape
data Tree sh a where
  Leaf   :: a -> Tree () a
  Branch :: (TreeShape m, TreeShape n) => Tree m a -> Tree n a -> Tree (m,n) a

instance Show a => Show (Tree sh a) where
  show (Leaf a) = show a
  show (Branch s t) = printf "{%s,%s}" (show s) (show t)

{--
instance TreeShape sh => Functor (Tree sh) where
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Branch s t) = Branch (fmap f s) (fmap f t)

instance TreeShape sh => Foldable (Tree sh) where
  -- (a -> m) -> Tree n a -> m
  foldMap f (Leaf a)     = f a
  foldMap f (Branch s t) = foldMap f s `mappend` foldMap f t

--
-- Trees have to have the same shape.
--
instance TreeShape sh => Applicative (Tree sh) where
  pure a                          = replicateT a
  Leaf fa <*> Leaf a              = Leaf (fa a)
  (Branch fs ft) <*> (Branch s t) = Branch (fs <*> s) (ft <*> t)

instance TreeShape sh => Traversable (Tree sh) where
  traverse f (Leaf a)     = Leaf   <$> f a
  traverse f (Branch s t) = Branch <$> traverse f s <*> traverse f t
--}


{--}

#define TYPE Tree
#include "instances-inc.hs"
instance EncodeF (Tree ()) where
  type Enc (Tree ()) = Id
  encode (Leaf a) = Id a
  decode (Id a)   = Leaf a

instance (TreeShape m, TreeShape n, EncodeF (Tree m), EncodeF (Tree n))
         => EncodeF (Tree (m,n)) where
  type Enc (Tree (m,n)) = Tree m :*: Tree n
  encode (Branch s t) = s :*: t
  decode (s :*: t)    = Branch s t

--}

tree1 :: Tree ((), ((), ())) Integer
tree1 = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))

tree2 :: Tree ((), ((), ())) Integer
tree2 = Branch (Leaf 4) (Branch (Leaf 5) (Leaf 6))

instance Foldable ZipList where
  -- ZipList m -> m
  fold = fold . getZipList

instance Traversable ZipList where
  -- traverse :: (a -> f b) -> ZipList a -> f (Ziplist b)
  traverse f (ZipList [])     = pure $ ZipList []
  traverse f (ZipList (x:xs)) = cons <$> f x <*> traverse f (ZipList xs)
    where
      cons x (ZipList xs) = ZipList (x:xs)
instance Show a => Show (ZipList a) where
  show (ZipList xs) = show xs

--
-- Bottom-up trees
--

#define TYPE TB
#include "instances-inc.hs"

data TB depth a where
   LB :: a -> TB Z a
   BB :: TB n (Pair a) -> TB (S n) a

instance Show a => Show (TB depth a) where
  show (LB a)   = printf "LB %s" (show a)
  show (BB t) = printf "BB (%s)" (show t)

instance EncodeF (TB Z) where
  type Enc (TB Z) = Id
  encode (LB a) = Id a
  decode (Id a) = LB a

instance EncodeF (TB n) => EncodeF (TB (S n)) where
  type Enc (TB (S n)) = TB n :.: Pair
  encode (BB t) = O t
  decode (O t) = BB t

treebu1 :: TB Two Int
treebu1 = BB (BB (LB ((1 :# 2) :# (3 :# 4))))

treebu2 :: TB Two Int
treebu2 = BB (BB (LB ((5 :# 6) :# (7 :# 8))))

--
-- Generalised dot products. Works on Vec, Pair, Tree (and much, much more!)
--
dot :: (Num a, Foldable f, Applicative f) => f a -> f a -> a
dot = dotGen (Product, getProduct) (Sum, getSum)

dotGen :: (Foldable f, Applicative f, Monoid p, Monoid s)
       => (a -> p, p -> a) -> (a -> s, s-> a) -> f a -> f a -> a
dotGen (pinject, pproject) (sinject, sproject) x y =
   sproject . fold . fmap (sinject . pproject) $ liftA2 mappend px py
  where
    px = fmap pinject x
    py = fmap pinject y
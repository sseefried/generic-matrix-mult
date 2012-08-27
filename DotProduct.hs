{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE CPP, TypeOperators, TypeFamilies #-}
module DotProduct where

import Prelude hiding (replicate, foldl, sum)
import Data.Traversable
import Control.Applicative
import Data.Foldable hiding (toList, sum)
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

infixr 5 `Cons`

type One   = S Z
type Two   = S One
type Three = S Two

{--
instance Foldable (Vec n) where
  -- foldMap :: Monoid m => (a -> m) -> Vec n a -> m
  foldMap f Nil         = mempty
  foldMap f (Cons x xs) = f x `mappend` foldMap f xs

instance Functor (Vec n) where
  fmap f Nil           = Nil
  fmap f (x `Cons` xs) = f x `Cons` fmap f xs

--
-- Vecs have to have the same length
--

instance Applicative (Vec Z) where
  pure _ = Nil
  Nil <*> Nil                       = Nil

instance Applicative (Vec n) => Applicative (Vec (S n)) where
  pure a = a `Cons` pure a
  (fa `Cons` fas) <*> (a `Cons` as) = fa a `Cons` (fas <*> as)

instance Traversable (Vec n) where
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
--}


{--

#define TYPE Tree
#include "instances-inc.hs"
instance EncodeF (Tree ()) where
  type Enc (Tree ()) = Id
  encode (Leaf a) = Id a
  decode (Id a)   = Leaf a

instance (EncodeF (Tree m), EncodeF (Tree n))
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

dot' :: (Num a, Foldable f, Applicative f) => f a -> f a -> a
dot' x y = foldSum $ liftA2 (*) x y
  where foldSum = getSum . fold . fmap Sum


dotGen :: (Foldable f, Applicative f, Monoid p, Monoid s)
       => (a -> p, p -> a) -> (a -> s, s-> a) -> f a -> f a -> a
dotGen (pinject, pproject) (sinject, sproject) x y =
   sproject . fold . fmap (sinject . pproject) $ liftA2 mappend px py
  where
    px = fmap pinject x
    py = fmap pinject y

-- dotMult :: (Num a, Applicative f) => f a -> f a -> f a
-- dotMult x y = fmap getProduct $ liftA2 mappend (fmap Product x) (fmap Product y)
--
-- dotSum :: (Num a, Foldable f, Functor f) => f a ->  a
-- dotSum x  = getSum . fold . fmap Sum $ x
--
-- dot' x y = dotSum $ dotMult x y

c2 = (.)(.)(.)

-- dotXX f m x y = getSum . fold $ liftA2 f (fmap m x) (fmap m y)



class DotX f where
  dotX :: (Monoid m) => (b -> b -> m) -> (a -> b) -> f a -> f a -> m

instance DotX (TB Z) where
  dotX f m (LB a) (LB b) = f (m a) (m b)

instance DotX (TB n) => DotX (TB (S n)) where
  dotX f m (BB s) (BB t) = dotX f' m' s t
   where
    m' (a :# b)            = m a :# m b
    f' (a :# b) (a' :# b') = f a a' `mappend` f b b'

dot''' :: (DotX (TB n), Num a) => TB n a -> TB n a -> a
dot''' = getSum `c2` dotX ((Sum . getProduct) `c2` mappend) Product

dotTBMult :: (a -> a -> a) -> TB n a -> TB n a -> TB n a
dotTBMult f (BB s) (BB t) = BB $ dotTBMult (pairLift f) s t
  where
    pairLift f (a :# b) (a' :# b') = f a a' :# f b b'

dotTBMult2 :: (EncodeF (TB n), Applicative (Enc (TB n))) => (a -> a -> a)
          -> TB (S n) a -> TB (S n) a -> TB (S n) a
dotTBMult2 f (BB s) (BB t) = BB $ liftA2 (<*>) (liftA2 (<*>) (pure $ f :# f) s) t


dotTBGen :: (a -> a -> a) -> (a -> a -> a) -> TB n a -> TB n a -> a
dotTBGen f g (LB a) (LB b) = f a b
dotTBGen f g (BB s) (BB t) = aux f g s t
  where
    aux :: (a -> a -> a) -> (a -> a -> a) -> TB n (Pair a) -> TB n (Pair a) -> a
    aux f g s t = let (a :# b) = dotTBGen (liftA2 f) (liftA2 g) s t in  g a b

dotTB :: Num a => TB n a -> TB n a -> a
dotTB x y = dotTBGen (*) (+) x y

class Lifty f where
  lifty :: (a -> b -> c) -> f a -> f b -> f c

instance (Lifty (TB n), EncodeF (TB n), Applicative (Enc (TB n))) => Lifty (TB (S n)) where
--  lifty :: (a -> b -> c) -> TB (S n) a -> TB (S n) b -> TB (S n) c
  lifty f (BB s) (BB t) = BB $ (pure $ f :# f) `app` s `app` t
    where app a b = lifty (<*>) a b


zipWithV :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWithV f Nil Nil = Nil
zipWithV f (Cons x xs) (Cons y ys) = f x y `Cons` zipWithV f xs ys

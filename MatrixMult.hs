{-# LANGUAGE TypeOperators, DeriveTraversable, DeriveFunctor, DeriveFoldable, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances, CPP, GADTs, EmptyDataDecls, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
--
-- The most minimal file for generic
-- matrix multiplication.
--
module MatrixMult where

-- standard imports
import Data.Traversable
import Control.Applicative
import Data.Foldable
import Data.Monoid
-- friends

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
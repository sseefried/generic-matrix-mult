--
-- In this module I'm playing around with Conal's suggestion that we
-- raise all data types up to type level and then specialize some type
-- parameters to ()
--

{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE CPP, TypeOperators, TypeFamilies #-}
module DotProductKinds where

import Prelude hiding (replicate, foldl, sum)
import Data.Traversable
import Control.Applicative
import Data.Foldable hiding (toList, sum)
import Data.Monoid
import Text.Printf

--friends
import FunctorCombinator


--
-- Vecs with length encoded. Prevents us from taking the dot product of two lists of
-- unequal length.
--
-- It also helps ensure that the Applicative instance for 'Vec's contains total functions.
--
data Vec n a where
  Nil  :: Vec Nil a
  Cons :: a -> Vec n a -> Vec (Cons () n) a

--
-- Constructors of Vec lifted to the type-level
--
type family Vec n
data Nil
data Cons x xs




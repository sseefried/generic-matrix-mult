{-# LANGUAGE ScopedTypeVariables, GADTs, EmptyDataDecls, CPP, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies, UndecidableInstances #-}

module Vec where

import Text.Printf
import Control.Applicative
import Data.Foldable hiding (toList)
import Data.Traversable

import GenericType
import Identity

data S n
data Z

type One   = S Z
type Two   = S One
type Three = S Two

infixr 5 `Cons`

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

--------------
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
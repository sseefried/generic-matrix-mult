{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveFunctor, DeriveFoldable #-} 
{-# LANGUAGE DeriveTraversable #-}
module FunctorCombinator where
  
import Prelude hiding (concat, sequence, zip, unzip)
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.Applicative
import Control.Arrow

data Pair a = a :# a deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Id a = Id a deriving (Show, Functor, Foldable, Traversable)

-- Sum types
data (f :+: g) a = InL (f a) | InR (g a)   deriving (Show, Functor, Foldable, Traversable)

-- Product types
data (f :*: g) a = f a :*: g a             deriving (Show, Functor, Foldable, Traversable)

-- Type composition
newtype (g :.: f) a = O { unO :: g (f a) } deriving (Show, Functor, Foldable, Traversable)

class EncodeF f where
  type Enc f :: * -> *
  encode :: f a -> Enc f a
  decode :: Enc f a -> f a

type Unit = Const ()




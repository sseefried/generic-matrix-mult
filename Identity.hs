{-# LANGUAGE TypeOperators #-}
module Identity where

import Control.Applicative

import GenericType

class Applicative f => Identity f where
  identity :: Num a => f (f a)

instance (Identity f, Identity g) => Identity (f :*: g) where
  identity = fmap (:*: pure 0) identity :*: fmap (pure 0 :*:) identity

instance Identity Unit where
  identity = Unit

instance Identity Id where
  identity = Id (Id 1)

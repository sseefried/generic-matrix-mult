instance (Functor (Enc (TYPE sh)), EncodeF (TYPE sh)) => Functor (TYPE sh) where
  fmap f = decode . fmap f . encode

instance (Foldable (Enc (TYPE sh)), EncodeF (TYPE sh)) => Foldable (TYPE sh) where
  fold = fold . encode

instance (Applicative (Enc (TYPE sh)), EncodeF (TYPE sh)) => Applicative (TYPE sh) where
  pure     = decode . pure
  fa <*> a = decode (encode fa <*> encode a)

instance (Traversable (Enc (TYPE sh)), EncodeF (TYPE sh)) => Traversable (TYPE sh) where
  traverse f = fmap decode . traverse f . encode

instance (Replicate (Enc (TYPE sh)), EncodeF (TYPE sh)) => Replicate (TYPE sh) where
  replicate = decode . replicate
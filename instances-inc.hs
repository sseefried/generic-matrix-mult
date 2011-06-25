instance (Functor (Enc (TYPE sh)), EncodeF (TYPE sh)) => Functor (TYPE sh) where
  fmap f a = decode (fmap f (encode a))

instance (Foldable (Enc (TYPE sh)), EncodeF (TYPE sh)) => Foldable (TYPE sh) where
  fold ma = fold (encode ma)

instance (Applicative (Enc (TYPE sh)), EncodeF (TYPE sh)) => Applicative (TYPE sh) where
  pure a = decode (pure a)
  fa <*> a = decode (encode fa <*> encode a)

instance (Traversable (Enc (TYPE sh)), EncodeF (TYPE sh)) => Traversable (TYPE sh) where
  traverse f x = fmap decode (traverse f (encode x))

instance (Replicate (Enc (TYPE sh)), EncodeF (TYPE sh)) => Replicate (TYPE sh) where
  replicate a = decode (replicate a)
{-
  One thing we must be keenly aware of on in all of these derivations is that there are
  really two definitions of all these functions one for type TB Z a and one for TB (S n) a

  Any occurrence of something of type TB Z a     must be of form     LB a
  Any occurrence of something of type TB (S n) a must be of form     BB t

-}



-- Let's see what fmap comes out to being.

-- Leaf case :: TB Z a
   fmap f (LB a)
==   {- fmap definition on TB Z -}
   decode . fmap f . encode $ LB a
==   {- encode definition -}
   decode . fmap f $ Id a
==   {- fmap defintion on Id -}
   decode $ Id (f a)
==   {- decode definition on Id -}
   LB (f a)

-- Branch case :: TB (S n) a
   fmap f (BB t)
==   {- fmap defintion on TB (S n) -}
   decode . fmap f . encode $ BB t
==   {- encode definition on TB (S n) -}
   decode . fmap f $ O t
==   {- fmap defintion on O -}
   decode . O . fmap (fmap f) $ t
==   {- decode definition on O -}
   BB . fmap (fmap f) $ t

-- Let's see what fold becomes
--
-- Leaf case :: TB Z a
--
   fold (LB a)
==   {- fold on TB Z -}
   fold . encode $ LB a
==   {- encode on TB Z -}
   fold (Id a)
== {- fold on Id -}
   a

--
-- Branch case :: TB (S n) a
--
   fold . encode $ BB t
==   {- encode on TB (S n) -}
   fold (O t)
==   {- fold on TB n :.: Pair -}
   fold . (fmap fold) $ t

--
-- Let's see what pure becomes
-- Leaf case. pure a :: TB Z a
    pure a
==    {- pure on TB n -}
    decode . pure $ a
==    {- pure on Id a -}
    decode (Id a)
==    {- decode on TB Z a -}
-- Branch case. pure a :: TB (S n) a
    pure a
==    {- pure on TB (S n) -}
    decode . pure $ a
==    {- pure on TB n :.: Pair -}
    decode . O . pure . pure $ a
==    {- rearrange -}
    decode . O . pure $ pure a
==    {- pure on Pair -}
    decode . O . pure $ (a :# a)
==    {- decode . O == BB -}
    BB . pure $ (a :# a)

{-

instance Applicative (TB Z) where
  pure = LB

instance Applicative (TB n) => Applicative (TB (S n)) where
  pure a = BB . pure $ (a :# a)

-}

-- Let's see what <*> becomes
-- Leaf case :: TB Z a
   LB fa <*> LB a
==    {- <*> on TB Z -}
   decode (encode (LB fa) <*> encode (LB a))
==    {- encode on TB Z -}
   decode (Id fa <*> Id a)
==    {- <*> on Id -}
   decode (Id (fa a))
==    {- decode on TB Z -}
   LB (fa a)

-- Branch case :: TB (S n) a
   BB ft <*> BB t
==   {- <*> on TB (S n) -}
   decode (encode (BB ft) <*> encode (BB t))
==   {- encode on TB (S n) -}
   decode (O ft <*> O t)
==   {- <*> on TB n :.: Pair -}
   decode (O $ (<*>) <$> ft <*> t)
==   {- decode . O == BB -}
   BB $ (<*>) <$> ft <*> t

{-

instance Applicative (TB Z) where
  pure = LB
  LB fa <*> LB a  = LB (fa a)

instance Applicative (TB n) => Applicative (TB (S n)) where
   pure a = BB . pure $ (a :# a)
   BB ft <*> BB t = BB $ (<*>) <$> ft <*> t

-}

-- Remember: liftA2 f a b = pure f <*> a <*> b
-- Let's see what liftA2 becomes
-- Leaf case
   liftA2 f (LB a) (LB b)
==   {- liftA2 def -}
   pure f <*> LB a <*> LB b
==   {- pure on TB n -}
   (decode . pure $ f) <*> LB a <*> LB b
==   {- <*> on TB n -}
   (decode (encode (decode . pure $ f)) <*> encode (LB a)) <*> LB b
==   {- encode . decode == id -}
   (decode (pure f) <*> encode (LB a)) <*> LB b
==   {- encode on TB n -}
   (decode (pure f) <*> Id a) <*> LB b
==   {- pure on Id -}
   (decode (Id f <*> Id a)) <*> LB b
==   {- <*> on Id -}
   (decode (Id (f a))) <*> LB b
==   {- decode on TB n -}
   LB (f a) <*> LB b
==   {- <*> on TB n -}
   decode (encode $ LB (f a)) <*> (encode $ LB b)
==   {- encode on TB n -}
   decode (Id (f a) <*> Id b)
==   {- <*> on Id -}
   decode (Id (f a b))
==  LB (f a b)

-- Branch case :: TB (S n) a
   pure f <*> BB s <*> BB t
==   {- pure on TB (S n) (derivation) -}
   BB (pure $ f :# f)) <*> BB s <*> BB t
==   {- <*> on TB (S n) (derivation) -}
   BB ((<*>) <$> (pure $ f :# f) <*> s) <*> BB t
==   {- liftA2 definition -}
   BB (liftA2 (<*>) (pure $ f :# f) s) <*> BB t
==   {- <*> on TB (S n) (derivation) -}
   BB ((<*>) <$> (liftA2 (<*>) (pure $ f :# f) s) <*> t)
==   {- liftA2 definition -}
   BB (liftA2 (<*>) (liftA2 (<*>) (pure $ f :# f) s) t
==   {- rearrange -}
   BB $ (pure $ f :# f) `app` s `app` t
      where app a b = liftA2 (<*>) a b


-- Let's see what dot becomes

-- Leaf case :: TB Z a

   dot (Leaf s) (Leaf t)
==   {- dot definition -}
   getSum . fold . fmap (Sum . getProduct) $
     liftA2 mappend (fmap Product (Leaf s)) (fmap Product (Leaf t))
==   {- -}
   getSum . fold . fmap (Sum . getProduct) $
     liftA2 mappend (decode . fmap Product . encode $ Leaf s) (decode . fmap Product . encode $ Leaf t)
==   {- fmap definition  and encode definition -}
   getSum . fold . fmap (Sum . getProduct) $
     liftA2 mappend (decode $ Id (Product s)) (decode $ Id (Product t))
==   {- decode definition -}
   getSum . fold . fmap (Sum . getProduct) $
     liftA2 mappend (Leaf (Product s)) (Leaf (Product t))
==   {- liftA2 definition -}
   getSum . fold . fmap (Sum . getProduct) $ Leaf (Product s `mappend` Product t)
==   {- mappend on Product monoid -}     
   getSum . fold . fmap (Sum . getProduct) $ Leaf (Product (s * t))
==   {- fmap definition -}   
   getSum . fold $ Leaf (Sum (s * t))
==   {- simplify -}    
   s * t
   
-- Branch case :: TB (S n) a
   dot (BB s) (BB t)
==   {- dot defintion -}
   getSum . fold . fmap (Sum . getProduct) $
     liftA2 mappend (fmap Product (BB s)) (fmap Product (BB t))
==   {- fmap on TB (S n) (derivation) -}
   getSum . fold . fmap (Sum . getProduct) $
     liftA2 mappend (BB . fmap (fmap Product) $ s) (BB . fmap (fmap Product) $ t)
==   {- liftA2 on TB (S n) (derivation) -}
   getSum . fold . fmap (Sum . getProduct) $
     BB (pure (mappend :# mappend)) `app` fmap (fmap Product) s `app` fmap (fmap Product) t
        where app a b = liftA2 (<*>) a b
==   {-  fmap on TB (S n) (derivation) -}
   getSum . fold $  BB $ fmap (fmap (Sum . getProduct)) $ 
     (pure (mappend :# mappend)) `app` fmap (fmap Product) s `app` fmap (fmap Product) t
        where app a b = liftA2 (<*>) a b
==   {-  fold on TB (S b) (derivation) -}
   getSum $ fold . (fmap fold) $ fmap (fmap (Sum . getProduct)) $ 
     (pure (mappend :# mappend)) `app` fmap (fmap Product) s `app` fmap (fmap Product) t
        where app a b = liftA2 (<*>) a b
==   {-  rearrange-}
   getSum $ fold . (fmap fold) . fmap (fmap (Sum . getProduct)) $ 
     (pure (mappend :# mappend)) `app` fmap (fmap Product) s `app` fmap (fmap Product) t
        where app a b = liftA2 (<*>) a b
==   {- fmap f (fmap g) == fmap (f . g) -}
   getSum $ fold . (fmap (fold . (fmap (Sum . getProduct)))) $ 
     (pure (mappend :# mappend)) `app` fmap (fmap Product) s `app` fmap (fmap Product) t
        where app a b = liftA2 (<*>) a b
==   {- rearrange -}
   getSum $ fold . (fmap (fold . (fmap (Sum . getProduct)))) $ 
     (liftA2 (<*>) (pure (mappend :# mappend)) (fmap (fmap Product) s)) `app` (fmap (fmap Product) t)
        where app a b = liftA2 (<*>) a b
==   {- rearrange -}
   getSum $ fold . (fmap (fold . (fmap (Sum . getProduct)))) $ 
     liftA2 (<*>) (liftA2 (<*>) (pure (mappend :# mappend)) 
                                (fmap (fmap Product) s))
                  (fmap (fmap Product) t)
==   {- rearrange -}
   foo (fmap (fold . (fmap (Sum . getProduct))))
       (<*>)
       (\s -> liftA2 (<*>) (pure (mappend :# mappend)) (fmap (fmap Product) s))
       (fmap (fmap Product))
    where 
      foo f g h j = getSum . fold . f $ liftA2 g (h s) (j t)

dotFoo s t = foo (fmap (Sum . getProduct)) mappend (fmap Product) (fmap Product)
  where 
    foo f g h j = getSum . fold . f $ liftA2 g (h s) (j t)

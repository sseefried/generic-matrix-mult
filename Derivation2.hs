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
   decode . fmap<Id> f . encode $ LB a
==   {- encode definition -}
   decode . fmap<Id> f $ Id a
==   {- fmap defintion on Id -}
   decode $ Id (f a)
==   {- decode definition on Id -}
   LB $ f a

-- Branch case :: TB (S n) a
   fmap f (BB t)
==   {- fmap defintion on TB (S n) -}
   decode . fmap<TB n:.:Pair> f . encode $ BB t
==   {- encode definition on TB (S n) -}
   decode . fmap<TB n:.:Pair f $ O t
==   {- fmap defintion on O -}
   decode . O . fmap<TB n> (fmap<Pair> f) $ t
==   {- decode definition on O -}
   BB . fmap<TB n> (fmap<Pair> f) $ t

{- 

instance Functor (TB Z) where
  fmap f (LB a) = LB $ f a

instance Functor (TB n) => Functor (TB (S n)) where
  fmap f (BB t) = BB . fmap (fmap f) $ t

-}


-- Let's see what fold becomes
--
-- Leaf case :: TB Z a
--
   fold (LB a)
==   {- fold on TB Z -}
   fold<Id> . encode $ LB a
==   {- encode on TB Z -}
   fold<Id> (Id a)
== {- fold on Id -}
   a

--
-- Branch case :: TB (S n) a
--
   fold<TB n:.:Pair> . encode $ BB t
==   {- encode on TB (S n) -}
   fold<TB n:.:Pair> (O t)
==   {- fold on TB n :.: Pair -}
   fold<TB n> . (fmap<TB n> fold<Pair>) $ t

{-
 
instance Foldable (TB Z) where
  fold (LB a) = a
  
instance Foldable (TB n) => Foldable (TB (S n)) where
  fold (BB t) = fold . (fmap fold) $ t
-}

--
-- Let's see what pure becomes
-- Leaf case. pure a :: TB Z a
    pure a
==    {- pure on TB n -}
    decode . pure<Id> $ a
==    {- pure on Id a -}
    decode (Id a)
==    {- decode on TB Z a -}
    LB a

-- Branch case. pure a :: TB (S n) a
    pure a
==    {- pure on TB (S n) -}
    decode . pure<TB n:.:Pair> $ a
==    {- pure on TB n :.: Pair -}
    decode . O . pure<TB n> . pure<Pair> $ a
==    {- rearrange -}
    decode . O . pure<TB n> $ pure<Pair> a
==    {- pure on Pair -}
    decode . O . pure<TB n> $ (a :# a)
==    {- decode . O == BB -}
    BB . pure<TB n> $ (a :# a)

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
   BB (pure $ pure<Pair> f)) <*> BB s <*> BB t
==   {- <*> on TB (S n) (derivation) -}
   BB (pure (<*>) <*> (pure $ pure<Pair> f) <*> s) <*> BB t
==   {- homomorphism law: pure f <*> pure x = pure (f x) -}
   BB ((pure $ (<*>) (pure<Pair> f)) <*> s) <*> BB t
==   {- <*> on TB (S n) (derivation) -}
   BB $ pure (<*>) <*> ((pure $ (<*>) (pure<Pair> f)) <*> s) <*> t
==   {- composition -}
   BB $ pure (.) <*> pure (<*>) <*> (pure $ (<*>) (pure<Pair> f)) <*> s <*> t
==   {- homomorphism -}
   BB $ pure ((.) (<*>)) <*> (pure $ (<*>) (pure<Pair> f)) <*> s <*> t
==   {- homomorphism -}
   BB $ pure ((.) (<*>) ((<*>) (pure<Pair> f))) <*> s <*> t

   BB $ pure ((.) (<*>) (\y -> (<*>) (pure<Pair> f) y)) <*> s <*> t

   BB $ pure (\x -> (<*>) ((\y -> (<*>) (pure<Pair> f) y) x)) <*> s <*> t

   BB $ pure (\x -> (<*>) ((<*>) (pure<Pair> f) x)) <*> s <*> t

   BB $ pure (\x -> (\z -> (<*>) ((<*>) (pure<Pair> f) x) z)) <*> s <*> t

   BB $ pure (\x -> (\z -> (<*>) ((pure<Pair> f) <*> x) z)) <*> s <*> t

   BB $ pure (\x -> (\z -> ((pure<Pair> f) <*> x) <*> z)) <*> s <*> t

   BB $ pure (\x -> (\z -> pure<Pair> f <*> x <*> z)) <*> s <*> t

   BB $ pure (\x -> (\z -> liftA2<Pair> f x z)) <*> s <*> t   

   BB $ pure (liftA2<Pair> f) <*> s <*> t   

   BB $ liftA2 (liftA2<Pair> f) s t   

{-




-}


-- == dotMult == 
-- Branch case :: TB Z a
   dotMult (BB s) (BB t)
==   {- dotMult -}
   fmap getProduct $ liftA2 mappend (fmap Product (BB s)) (fmap Product (BB t))
==   {- fmap on TB (S n) (derivation) -}  
   fmap getProduct $ liftA2 mappend (BB . fmap (fmap Product) $ s) (BB . fmap (fmap Product) $ t)
==   {- liftA2 on TB (S n) (derivation) -}
   fmap getProduct $ liftA2 (<*>) (liftA2 (<*>) (pure $ mappend :# mappend)
                                                (BB . fmap (fmap Product) $ s))
                                  (BB . fmap (fmap Product) $ t)
==   {- rearrange -}
   fmap getProduct $ liftA2 (<*>) (liftA2 (<*>) (BB result) (BB . fmap (fmap Product) $ s))
                                  (BB . fmap (fmap Product) $ t)
     where result = pure $ mappend :# mappend
==   {- rearrange -}
   fmap getProduct $ liftA2 (<*>) (liftA2 (<*>) (BB result) (BB . fmap (fmap Product) $ s))
                                  (BB . fmap (fmap Product) $ t)

foo a b = liftA2 (<*>) a b

   foo (BB s) (BB t)
==  
   liftA2 (<*>) (BB s) (BB t)
==   {- rearrange -}  
   BB app <*> BB s <*> BB t
      where (BB app) = pure (<*>)
== 
   (BB $ liftA2 (<*>) app s) <*> BB t
      where (BB app) = pure (<*>)
== 
   BB $ liftA2 (<*>) (liftA2 (<*>) app s) t
      where (BB app) = pure (<*>)
== 
   BB $ foo (liftA2 <*> app s) t
      where (BB app) = pure (<*>)
== 
   BB $ foo (foo app s) t
      where (BB app) = pure (<*>)
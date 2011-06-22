{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, DeriveFunctor, DeriveFoldable, TypeFamilies #-}
module MatrixMultiply where

import DotProduct


import Control.Applicative
import Data.Foldable hiding (toList)
import qualified Data.Foldable as F
import Data.Monoid
import Text.Printf

--transposeM :: (Nat m, Nat n) => ListMatrix m n a -> ListMatrix n m a
--transposeM  Nil                        = replicateL Nil
--transposeM  (Nil `Cons` _)             = Nil
--transposeM  m@((_ `Cons` _) `Cons` _)  = mapL headL m `Cons` (transposeM (mapL tailL m))

test1 :: List Two (List Three Integer)
test1 = (1 `Cons` 2 `Cons` 3 `Cons` Nil) `Cons`
        (4 `Cons` 5 `Cons` 6 `Cons` Nil) `Cons` Nil

test2 :: List Three (List Two Integer)
test2 = (1 `Cons` 2 `Cons` Nil) `Cons`
        (3 `Cons` 4 `Cons` Nil) `Cons`
        (5 `Cons` 6 `Cons` Nil) `Cons` Nil

--
--
-- 


--
-- The second matrix has already been tranposed.
--
mmult :: (Num a, Foldable f1, Foldable f2, Applicative f1, Applicative f2)
       => f1 (f2 a) -> f1 (f2 a) -> f1 (f1 a)
mmult m1 m2 = fmap (flip combine m1) m2
  where
    combine :: (Num a, Foldable f1, Foldable f2, Applicative f1, Applicative f2)
            => f2 a -> f1 (f2 a) -> f1 a
    combine inner = fmap (dot inner) 
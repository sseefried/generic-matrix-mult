{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, DeriveFunctor, DeriveFoldable, TypeFamilies #-}
module MatrixMultiply where

import DotProduct


import Control.Applicative
import Data.Foldable hiding (toList)
import qualified Data.Foldable as F
import Data.Monoid
import Data.Traversable
import Text.Printf

--
-- What is a matrix, Neo? 
--
-- A matrix is simply a collection within a collection. e.g. a list of lists, a list of trees, 
-- or a tree of trees
--
-- When we multiply two matrices together we multiply one type with its mirrored type.
--
-- e.g. If the first matrix is a list of trees, then the second must be a tree of lists.
--
--



--
-- Generalised transpose.
-- 
-- Note: This is just plain beautiful. When I wrote down the type signature I knew 
-- almost immediately I'd seen it before. Turns out it was in class Traversable! 
--
transpose :: (Traversable f1, Applicative f2) => f1 (f2 a) -> f2 (f1 a)
transpose = sequenceA 

mmult :: (Num a, Foldable f1, Foldable f2, Applicative f1, Applicative f2, Traversable f2)
       => f1 (f2 a) -> f2 (f1 a) -> f1 (f1 a)
mmult m1 m2 = fmap (flip combine m1) (transpose m2)
  where
    combine :: (Num a, Foldable f1, Foldable f2, Applicative f1, Applicative f2)
            => f2 a -> f1 (f2 a) -> f1 a
    combine inner = fmap (dot inner)

--
--  Tests
--

llmat1 :: List Two (List Three Integer)
llmat1 = (1 `Cons` 2 `Cons` 3 `Cons` Nil) `Cons`
        (4 `Cons` 5 `Cons` 6 `Cons` Nil) `Cons` Nil

llmat2 :: List Three (List Two Integer)
llmat2 = (1 `Cons` 2 `Cons` Nil) `Cons`
         (3 `Cons` 4 `Cons` Nil) `Cons`
         (5 `Cons` 6 `Cons` Nil) `Cons` Nil

ltmat1 :: List Two (Tree ((), ((), ())) Integer)
ltmat1 = (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) `Cons`
         (Branch (Leaf 4) (Branch (Leaf 5) (Leaf 6))) `Cons` Nil

ltmat2 :: Tree ((), ((), ())) (List Two Integer)
ltmat2 = Branch (Leaf (1 `Cons` 2 `Cons` Nil)) (Branch (Leaf (3 `Cons` 4 `Cons` Nil)) 
                                                      (Leaf (5 `Cons` 6 `Cons` Nil)))


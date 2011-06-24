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
-- A matrix is simply a collection of collections.
-- e.g. a vector of vectors, a vector of trees, or a tree of trees
--
-- When we multiply two matrices together we multiply one type with its mirrored type.
--
-- e.g. If the first matrix is a vector of trees, then the second must be a tree of vectors.
--
--
-- The result will be a vector of vectors.
--
-- I find this to be a very beautiful property. It has symmetries with what we know about the
-- dimensions of the resulting matrix, given the dimension of the input matrix.
-- eg. In mathematics, a 3x2 matrix multiplied with a 2x3 matrix gives you a 3x3 matrix.
--
-- In Haskell a 'Vec Two (Tree () Integer)' matrix multiplied by 'Tree () (Vec Three Integer)'
-- matrix gives us a 'Vec Two (Vec Three Integer)' matrix
--

--
-- Generalised transpose.
--
-- Note: This is just plain beautiful. When I wrote down the type signature I knew
-- I'd seen it before. Turns out it was in class Traversable!
--
transpose :: (Traversable f1, Applicative f2) => f1 (f2 a) -> f2 (f1 a)
transpose = sequenceA

mmult :: (Num a, Applicative f1, Applicative f2, Applicative f3, Traversable f1, Traversable f2)
       => f1 (f2 a) -> f2 (f3 a) -> f1 (f3 a)
mmult m1 m2 = fmap (flip (fmap . dot) (transpose m2)) m1 

--
--  Tests
--
vvmat23 :: Vec Two (Vec Three Integer)
vvmat23 = (1 `Cons` 2 `Cons` 3 `Cons` Nil) `Cons`
        (4 `Cons` 5 `Cons` 6 `Cons` Nil) `Cons` Nil

vvmat32 :: Vec Three (Vec Two Integer)
vvmat32 = (1 `Cons` 2 `Cons` Nil) `Cons`
         (3 `Cons` 4 `Cons` Nil) `Cons`
         (5 `Cons` 6 `Cons` Nil) `Cons` Nil
         
vvmat33 :: Vec Three (Vec Three Integer)
vvmat33 = (1 `Cons` 2 `Cons` 3 `Cons` Nil) `Cons`
          (4 `Cons` 5 `Cons` 6 `Cons` Nil) `Cons`
          (7 `Cons` 8 `Cons` 9 `Cons` Nil) `Cons` Nil

llmat23 :: ZipList (ZipList Integer)
llmat23 = ZipList [ZipList [1,2,3],
                   ZipList [4,5,6]]
           
llmat33 :: ZipList (ZipList Integer)
llmat33 = ZipList [ ZipList [1,2,3]
                  , ZipList [4,5,6]
                  , ZipList [7,8,9]]
                  
llmat35 :: ZipList (ZipList Integer)
llmat35 = ZipList [ ZipList [1,2,3,4,5]
                  , ZipList [6,7,8,9,10]
                  , ZipList [11,12,13,14,15]]

ltmat1 :: Vec Two (Tree ((), ((), ())) Integer)
ltmat1 = (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) `Cons`
         (Branch (Leaf 4) (Branch (Leaf 5) (Leaf 6))) `Cons` Nil

tlmat2 :: Tree ((), ((), ())) (Vec Two Integer)
tlmat2 = Branch (Leaf (1 `Cons` 2 `Cons` Nil)) (Branch (Leaf (3 `Cons` 4 `Cons` Nil))
                                                      (Leaf (5 `Cons` 6 `Cons` Nil)))
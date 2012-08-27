{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeOperators, UndecidableInstances #-}
module MatrixMultiply where

-- Standard libraries
import Control.Applicative
import Data.Foldable hiding (toList)
import qualified Data.Foldable as F
import Data.Monoid
import Data.Traversable
import Text.Printf

-- Friends
import DotProduct
import FunctorCombinator

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


class Applicative f => Identity f where
  identity :: Num a => f (f a)

{--
instance Identity (Vec Z) where
  identity = Nil

instance Identity (Vec n) => Identity (Vec (S n)) where
  identity = (1 `Cons` pure 0) `Cons` fmap (0 `Cons`) identity
-}
instance Identity (Tree ()) where
  identity = Leaf (Leaf 1)

instance (Identity (Tree m), Identity (Tree n)) => Identity (Tree (m,n)) where
  identity = Branch (fmap (`Branch` pure 0) identity) (fmap (pure 0 `Branch`) identity)
--}
----------------------------

{--}
instance (Identity (Enc (Vec n)), EncodeF (Vec n)) => Identity (Vec n) where
  identity = decode . fmap decode $ identity

instance (Identity f, Identity g) => Identity (f :*: g) where
  identity = fmap (:*: pure 0) identity :*: fmap (pure 0 :*:) identity

instance Identity Unit where
  identity = Unit

instance Identity Id where
  identity = Id (Id 1)

id1 :: Vec Three (Vec Three Int)
id1 = identity

id2 :: Tree ((),()) (Tree ((),()) Int)
id2 = identity

--}

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

vvmat35 :: Vec Three (Vec (S (S Three)) Integer)
vvmat35 = (1 `Cons` 2 `Cons` 3 `Cons` 4 `Cons` 5 `Cons` Nil) `Cons`
          (6 `Cons` 7 `Cons` 8 `Cons` 9 `Cons` 10 `Cons` Nil) `Cons`
          (11 `Cons` 12 `Cons` 13 `Cons` 14 `Cons` 15 `Cons` Nil) `Cons` Nil


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

vtmat23 :: Vec Two (Tree ((), ((), ())) Integer)
vtmat23 = (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) `Cons`
         (Branch (Leaf 4) (Branch (Leaf 5) (Leaf 6))) `Cons` Nil

tvmat32 :: Tree ((), ((), ())) (Vec Two Integer)
tvmat32 = Branch (Leaf (1 `Cons` 2 `Cons` Nil)) (Branch (Leaf (3 `Cons` 4 `Cons` Nil))
                                                      (Leaf (5 `Cons` 6 `Cons` Nil)))

tvmat33 :: Tree ((), ((), ())) (Vec Three Integer)
tvmat33 = Branch (Leaf (1 `Cons` 2 `Cons` 3 `Cons` Nil))
         (Branch (Leaf (4 `Cons` 5 `Cons` 6 `Cons` Nil))
                 (Leaf (7 `Cons` 8 `Cons` 9 `Cons` Nil)))

vtmat33 :: Vec Three (Tree ((), ((), ())) Integer)
vtmat33 = (Branch (Leaf 10) (Branch (Leaf 11) (Leaf 12))) `Cons`
          (Branch (Leaf 13) (Branch (Leaf 14) (Leaf 15))) `Cons`
          (Branch (Leaf 16) (Branch (Leaf 17) (Leaf 18))) `Cons` Nil


vbmat24 :: Vec Two (TB Two Integer)
vbmat24 = (BB $ BB $ LB ((1:#2):#(3:#4))) `Cons`
          (BB $ BB $ LB ((5:#6):#(7:#8))) `Cons` Nil

vbmat42 :: TB Two (Vec Two Integer)
vbmat42 = BB $ BB $ LB (((9 `Cons` 10 `Cons` Nil) :# (11 `Cons` 12 `Cons` Nil)) :#
                        ((13 `Cons` 14 `Cons` Nil) :# (15 `Cons` 16 `Cons` Nil)))
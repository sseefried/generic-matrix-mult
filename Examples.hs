{-#  #-}
module Examples where

import Control.Applicative

import MatrixMult
import Vec
import Tree


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


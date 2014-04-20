module Functor where

import Tree
import Prelude hiding (Functor(..), Maybe(..))

-- data [] a = a : [a] | []
data Maybe a = Just a | Nothing deriving (Show, Eq)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = undefined

instance Functor Maybe where
  fmap = undefined

-- Might break tree ordering properties, but still a valid functor
instance Functor BST where
  fmap = undefined

fmaybe = Just 3
flist  = [1, 4, 2, 3]
fbst   = fromList flist

square :: (Functor f, Num a) => f a -> f a
square = undefined
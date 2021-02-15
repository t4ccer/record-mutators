module Data.Mergeable.Class where

import           Data.Functor.Identity

merge1 :: Identity a -> Maybe a -> Identity a
merge1 _ (Just x)           = Identity x
merge1 (Identity x) Nothing = Identity x

class Mergeable m where
  merge :: m Identity -> m Maybe -> m Identity


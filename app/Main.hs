{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import           Data.Default
import           Data.Functor.Identity
import           Data.Mergeable
import           GHC.Generics

data User m = User
  { name :: m String
  , age  :: m Int
  , uid  :: Int
  }
  deriving (Generic)
deriving instance Show (User Identity)
deriving instance Show (User Maybe)
$(mkMergeable ''User)

userMod :: User Maybe
userMod = def {name = pure "Mike"}

user :: User Identity
user = User (pure "John") (pure 42) 1

main :: IO ()
main = do
  print user
  print userMod
  print $ merge user userMod


{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Main where

import           Data.Default
import           Data.Functor.Identity
import           Data.Mergeable
import           GHC.Generics

data User m = User
  { userId        :: Int
  , userFirstName :: m String
  , userLastName  :: m String
  -- milion other fields
  }
  deriving (Generic)

$(mkMergeable ''User)

type UserData = User Identity
type UserMutation = User Maybe

deriving instance Show UserData
deriving instance Show UserMutation

userMod :: UserMutation
userMod = def {userFirstName = pure "Mike"}

user :: UserData
user = User  1 (pure "John") (pure "Smith")

main :: IO ()
main = do
  print user
  print userMod
  print $ merge user userMod

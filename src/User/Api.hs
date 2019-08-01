{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module User.Api where

import           Control.Lens        hiding ((.=))
import           Control.Monad.State
import           Data.Aeson
import           Data.Maybe
import           GHC.Generics
import           System.Random
import           User.Database

data CreateUserRequest = CreateUserRequest
  { _curEmail    :: String
  , _curPassword :: String
  , _curAge      :: Int
  } deriving (Show, Generic)

instance FromJSON CreateUserRequest where
  parseJSON (Object v) =
    CreateUserRequest <$> v .: "email"
                      <*> v .: "password"
                      <*> v .: "age"
  parseJSON _ = mzero

data GetUserResponse = GetUserResponse
  { _gurId    :: Int
  , _gurEmail :: String
  , _gurAge   :: Int
  } deriving (Show, Generic)

fromUser :: User -> GetUserResponse
fromUser (User id email _ age) = GetUserResponse id email age

instance ToJSON GetUserResponse where
  toJSON (GetUserResponse id email age) =
      object [ "id" .= id
             , "email" .= email
             , "age" .= age
             ]

data LoginRequest = LoginRequest
    { _lrEmail    :: String
    , _lrPassword :: String
    } deriving (Show, Generic)

instance FromJSON LoginRequest where
  parseJSON (Object v) =
    LoginRequest <$> v .: "email"
                 <*> v .: "password"
  parseJSON _ = mzero

makeLenses ''CreateUserRequest
makeLenses ''GetUserResponse
makeLenses ''LoginRequest

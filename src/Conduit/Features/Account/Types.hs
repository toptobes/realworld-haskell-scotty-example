module Conduit.Features.Account.Types where

import Data.Aeson (FromJSON, ToJSON (toJSON), object, (.=))
import Data.Aeson.Types (Value)

newtype UserID = UserID { unUserID :: Int64 } 
  deriving newtype (Show, Read, ToJSON, Num)

data UserAuth = UserAuth
  { userEmail :: Text
  , userName  :: Text
  , userToken :: Text
  , userBio   :: Maybe Text
  , userImage :: Maybe Text
  }

instance ToJSON UserAuth where
  toJSON :: UserAuth -> Value
  toJSON UserAuth {..} = object
    [ "username" .= userName
    , "email"    .= userEmail
    , "token"    .= userToken
    , "bio"      .= userBio
    , "image"    .= userImage
    ]

newtype InUserObj a = InUserObj
  { user :: a
  } deriving (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data UserProfile = UserProfile
  { userName     :: Text
  , userBio      :: Maybe Text
  , userImage    :: Maybe Text
  , userFollowed :: Bool
  }

instance ToJSON UserProfile where
  toJSON :: UserProfile -> Value
  toJSON UserProfile {..} = object
    [ "username"  .= userName
    , "image"     .= userImage
    , "bio"       .= userBio
    , "following" .= userFollowed
    ]

newtype InProfileObj a = InProfileObj
  { profile :: a
  } deriving (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

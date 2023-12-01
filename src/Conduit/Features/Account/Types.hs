module Conduit.Features.Account.Types where

import Conduit.Utils (InObj(..))
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson.Types (Value)

newtype UserID = UserID { unID :: Int64 } 
  deriving newtype (Show, Read, Eq, ToJSON)

data UserAuth = UserAuth
  { userEmail :: Text
  , userName  :: Text
  , userToken :: Text
  , userBio   :: Maybe Text
  , userImage :: Text
  } deriving (Show)

instance ToJSON UserAuth where
  toJSON :: UserAuth -> Value
  toJSON UserAuth {..} = object
    [ "username" .= userName
    , "email"    .= userEmail
    , "token"    .= userToken
    , "bio"      .= userBio
    , "image"    .= userImage
    ]

inUserObj :: obj -> InObj obj
inUserObj = InObj "user"

data UserProfile = UserProfile
  { userName     :: Text
  , userBio      :: Maybe Text
  , userImage    :: Text
  , userFollowed :: Bool
  } deriving (Show)

instance ToJSON UserProfile where
  toJSON :: UserProfile -> Value
  toJSON UserProfile {..} = object
    [ "username"  .= userName
    , "image"     .= userImage
    , "bio"       .= userBio
    , "following" .= userFollowed
    ]

inProfileObj :: obj -> InObj obj
inProfileObj = InObj "profile"

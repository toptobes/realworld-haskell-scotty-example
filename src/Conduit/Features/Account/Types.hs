module Conduit.Features.Account.Types where

import Conduit.Utils (InObj(..))
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson.Types (Value)

newtype UserID = UserID { unID :: Int64 } 
  deriving newtype (Show, Read, Eq, ToJSON)

data UserAuth = UserAuth
  { email :: Text
  , name  :: Text
  , token :: Text
  , bio   :: Maybe Text
  , image :: Text
  } deriving (Show)

instance ToJSON UserAuth where
  toJSON :: UserAuth -> Value
  toJSON UserAuth {..} = object
    [ "username" .= name
    , "email"    .= email
    , "token"    .= token
    , "bio"      .= bio
    , "image"    .= image
    ]

inUserObj :: obj -> InObj obj
inUserObj = InObj "user"

data UserProfile = UserProfile
  { name     :: Text
  , bio      :: Maybe Text
  , image    :: Text
  , followed :: Bool
  } deriving (Show)

instance ToJSON UserProfile where
  toJSON :: UserProfile -> Value
  toJSON UserProfile {..} = object
    [ "username"  .= name
    , "image"     .= image
    , "bio"       .= bio
    , "following" .= followed
    ]

inProfileObj :: obj -> InObj obj
inProfileObj = InObj "profile"

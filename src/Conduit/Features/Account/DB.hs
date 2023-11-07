module Conduit.Features.Account.DB where

import Database.Selda

data UserTable = UserTable
  { user_id  :: ID UserTable
  , username :: Text
  , password :: Text
  , email    :: Text
  , bio      :: Maybe Text
  , image    :: Maybe Text
  } deriving (Generic, SqlRow)
  
usersTable :: Table UserTable
usersTable = table "users" 
  [ #user_id  :- autoPrimary
  , #username :- unique
  , #email    :- unique
  ]

data FollowsTable = FollowsTable
  { followee_id :: ID UserTable
  , follower_id :: ID UserTable
  } deriving (Generic, SqlRow)

followsTable :: Table FollowsTable
followsTable = table "follows" 
  [ (#followee_id :+ #follower_id) :- primary
  ]

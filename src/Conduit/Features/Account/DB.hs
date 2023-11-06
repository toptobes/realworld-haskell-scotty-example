module Conduit.Features.Account.DB where

import Database.Selda (Attr((:-)), SqlRow, Table, table, autoPrimary)
import Database.Selda.SqlType (ID)

data UserTable = UserTable
  { userID    :: ID UserTable
  , userName  :: Text
  , userPass  :: Text
  , userEmail :: Text
  , userBio   :: Maybe Text
  , userImage :: Maybe Text
  } deriving (Generic, SqlRow)
  
usersTable :: Table UserTable
usersTable = table "users" 
  [ #userID :- autoPrimary
  ]

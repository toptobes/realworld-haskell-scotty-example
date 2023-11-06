module Conduit.Features.Account.DB where

import Database.Selda (Attr ((:-)), SqlRow, Table, autoPrimary, table, unique)
import Database.Selda.SqlType (ID)

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
  [ #user_id :- autoPrimary
  , #email   :- unique
  ]

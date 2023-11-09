{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances  #-}

module Conduit.Features.Account.DB where

import Conduit.Features.Account.Types (UserID(..))
import Database.Esqueleto.Experimental (PersistEntity(..), SqlBackend, ToBackendKey, fromSqlKey, toSqlKey)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

-- data UserTable = UserTable
--   { user_id  :: ID UserTable
--   , username :: Text
--   , password :: Text
--   , email    :: Text
--   , bio      :: Maybe Text
--   , image    :: Maybe Text
--   } deriving (Generic, SqlRow)
  
-- usersTable :: Table UserTable
-- usersTable = table "users" 
--   [ #user_id  :- autoPrimary
--   , #username :- unique
--   , #email    :- unique
--   ]

-- data FollowsTable = FollowsTable
--   { followee_id :: ID UserTable
--   , follower_id :: ID UserTable
--   } deriving (Generic, SqlRow)

-- followsTable :: Table FollowsTable
-- followsTable = table "follows" 
--   [ (#followee_id :+ #follower_id) :- primary
--   ]

share [mkPersist sqlSettings, mkMigrate "migrateAccountTables"] [persistLowerCase|
  User
    username Text
    password Text
    email Text
    bio   Text Maybe
    image Text Maybe
    
    UniqueUsername username 
    UniqueEmail email
  
  Follow
    followeeID UserId sql=followee_id OnDeleteCascade
    followerID UserId sql=follower_id OnDeleteCascade
    Primary followeeID followerID
|]

sqlKey2userID :: (ToBackendKey SqlBackend a) => Key a -> UserID
sqlKey2userID = UserID . fromSqlKey

userID2sqlKey :: (ToBackendKey SqlBackend a) => UserID -> Key a
userID2sqlKey = toSqlKey . unUserID

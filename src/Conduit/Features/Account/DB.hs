{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances  #-}

module Conduit.Features.Account.DB where

import Conduit.Features.Account.Types (UserID(..))
import Database.Esqueleto.Experimental (PersistEntity(..), SqlBackend, ToBackendKey, fromSqlKey, toSqlKey)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

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
    followerID UserId sql=follower_id OnDeleteCascade
    followeeID UserId sql=followee_id OnDeleteCascade
    Primary followeeID followerID
|]

sqlKey2userID :: (ToBackendKey SqlBackend a) => Key a -> UserID
sqlKey2userID = UserID . fromSqlKey

userID2sqlKey :: (ToBackendKey SqlBackend a) => UserID -> Key a
userID2sqlKey = toSqlKey . unUserID

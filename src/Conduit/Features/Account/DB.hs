{-# LANGUAGE QuasiQuotes, TemplateHaskell, UndecidableInstances #-}

module Conduit.Features.Account.DB where

import Conduit.Features.Account.Types (UserID(..))
import Database.Esqueleto.Experimental (Key)
import Database.Esqueleto.Experimental qualified as E
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Conduit.DB.Types (SqlKey(..))

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

instance SqlKey User UserID where
  sqlKey2ID :: Key User -> UserID
  sqlKey2ID = UserID . E.fromSqlKey

  id2sqlKey :: UserID -> Key User
  id2sqlKey = E.toSqlKey . unID

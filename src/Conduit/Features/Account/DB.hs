{-# LANGUAGE QuasiQuotes, TemplateHaskell, UndecidableInstances #-}

module Conduit.Features.Account.DB where

import Conduit.Features.Account.Types (UserID(..), UserProfile (..))
import Database.Esqueleto.Experimental (Entity (..), Value (..))
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Conduit.DB.Types (deriveSqlKey)

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

$(deriveSqlKey ''User ''UserID)

mkProfile :: Entity User -> Value Bool -> UserProfile
mkProfile (Entity _ user) (Value followed) = UserProfile
  { userName  = user.userUsername
  , userBio   = user.userBio
  , userImage = user.userImage
  , userFollowed = followed
  }

{-# LANGUAGE QuasiQuotes, TemplateHaskell, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conduit.Features.Account.DB where

import Conduit.DB.Core (deriveSqlKey)
import Conduit.Features.Account.Types (UserID(..), UserProfile(..))
import Database.Esqueleto.Experimental (Entity(..), Value(..))
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateAccountTables"] [persistLowerCase|
  User
    username Text
    password Text
    email Text
    bio   Text Maybe
    image Text
    
    UniqueUsername username 
    UniqueEmail email
  
  Follow
    followerID UserId sql=follower_id OnDeleteCascade
    followedID UserId sql=followed_id OnDeleteCascade
    Primary followerID followedID
|]

$(deriveSqlKey ''User ''UserID)

mkProfile :: Entity User -> Value Bool -> UserProfile
mkProfile (Entity _ user) (Value followed) = UserProfile
  { name  = user.userUsername
  , bio   = user.userBio
  , image = user.userImage
  , followed = followed
  }

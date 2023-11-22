module Conduit.Features.Account.Common.QueryUserFollows (queryIfUserFollows) where

import Conduit.DB.Core (id2sqlKey)
import Conduit.DB.Utils (suchThat)
import Conduit.Features.Account.DB (Follow, User, UserId)
import Conduit.Features.Account.Types (UserID)
import Database.Esqueleto.Experimental (Entity, SqlExpr, Value, exists, from, just, table, val, (&&.), (==.))

queryIfUserFollows :: (ComparableUserEntity a) => SqlExpr a -> Maybe UserID -> SqlExpr (Value Bool)
queryIfUserFollows follower followedID = exists 
  $ void 
  $ from (table @Follow)
    `suchThat` \f ->
      (just f.followedID ==. val (followedID <&> id2sqlKey)) &&. (follower `compIDWith` f.followerID)

-- Not sure if I'm being stupid, but I can't find a method that turns an Entity inside SqlExpr into a Maybe
-- so this is just a quick little workaround "for now"

class ComparableUserEntity a where
  compIDWith :: SqlExpr a -> SqlExpr (Value UserId) -> SqlExpr (Value Bool)

instance ComparableUserEntity (Entity User) where
  compIDWith :: SqlExpr (Entity User) -> SqlExpr (Value UserId) -> SqlExpr (Value Bool)
  compIDWith u otherID = otherID ==. u.id

instance ComparableUserEntity (Maybe (Entity User)) where
  compIDWith :: SqlExpr (Maybe (Entity User)) -> SqlExpr (Value UserId) -> SqlExpr (Value Bool)
  compIDWith u otherID = just otherID ==. u.id

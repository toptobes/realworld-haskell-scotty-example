module Conduit.Features.Account.Exports.QueryAssociatedUser (queryAssociatedUser) where

import Prelude hiding (on)
import Conduit.Features.Account.DB (User)
import Conduit.Features.Account.Exports.QueryUserFollows (queryIfUserFollows)
import Conduit.Features.Account.Types (UserID)
import Database.Esqueleto.Experimental (Entity, PersistEntity, SqlExpr, SqlQuery, Value(..), from, on, table, (:&)(..), innerJoin)

queryAssociatedUser 
  :: ∀ table. (PersistEntity table) 
  => Maybe UserID
  -> (SqlExpr (Entity table) -> SqlExpr (Entity User) -> SqlExpr (Value Bool)) 
  -> SqlQuery (SqlExpr (Entity table) :& SqlExpr (Entity User), SqlExpr (Value Bool))
queryAssociatedUser userID relation = do
  joined@(_ :& u) <- from $
    table @table
      `innerJoin` 
    table @User
      `on` \(a :& b) -> relation a b

  let follows = queryIfUserFollows u userID

  pure (joined, follows)

module Conduit.Features.Articles.Common.QueryFavStats where

import Conduit.DB.Core (id2sqlKey)
import Conduit.DB.Utils (suchThat)
import Conduit.Features.Account.Types (UserID)
import Conduit.Features.Articles.DB (Article, Favorite)
import Database.Esqueleto.Experimental (Entity, SqlExpr, Value, exists, from, just, subSelectCount, table, val, (&&.), (==.))

queryFavStats :: Maybe UserID -> SqlExpr (Entity Article) -> (SqlExpr (Value Bool), SqlExpr (Value Int))
queryFavStats userID a = (favorited, numFavs)
  where
    favorited = exists $ void $ from (table @Favorite)
      `suchThat` \f' ->
        (a.id ==. f'.article) &&. (just f'.user ==. val (userID <&> id2sqlKey))

    numFavs = subSelectCount @Int $ from (table @Favorite)
      `suchThat` \f' ->
        a.id ==. f'.article

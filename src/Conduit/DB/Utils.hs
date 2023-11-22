module Conduit.DB.Utils where

import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Database.Esqueleto.Experimental (SqlExpr, SqlQuery, Value, where_)

-- | Temporarily sets the time to some irrelevant number for typing reasons.
--   Used for rows where the DB sets the timestamp using some trigger.
dbTimeNow :: UTCTime
dbTimeNow = UTCTime (fromGregorian 1 0 0) (secondsToDiffTime 0)

-- | Shorthand for DB queries that select from a table just applying some filter
--
-- > select $ do 
-- >   e <- from table `suchThat` \e -> e.id ==. ???
-- >   ...
infixl 5 `suchThat`
suchThat :: SqlQuery a -> (a -> SqlExpr (Value Bool)) -> SqlQuery a
suchThat entity predicate = do
  e <- entity
  where_ (predicate e)
  pure e

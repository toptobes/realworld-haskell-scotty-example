module Conduit.DB.Utils where

import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Database.Esqueleto.Experimental (SqlExpr, SqlQuery, Value, where_)

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 1 0 0) (secondsToDiffTime 0)

infixl 5 `suchThat`
suchThat :: SqlQuery a -> (a -> SqlExpr (Value Bool)) -> SqlQuery a
suchThat entity predicate = do
  e <- entity
  where_ (predicate e)
  pure e

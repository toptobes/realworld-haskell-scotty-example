module Conduit.DB
  ( ConnectionOps(..)
  , initSelda
  , DBPool
  ) where

import Conduit.Features.Account.DB (usersTable)
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Database.Selda (MonadSelda, tryCreateTable)
import Database.Selda.Backend (SeldaConnection)
import Database.Selda.PostgreSQL (PG, PGConnectInfo(..), pgOpen, seldaClose, withPostgreSQL)

type DBPool = Pool (SeldaConnection PG)

data ConnectionOps = ConnectionOps
  { connStr  :: !Text
  , connLife :: !Double
  , connMax  :: !Int
  }

initSelda :: ConnectionOps -> IO DBPool
initSelda ConnectionOps {..} = do
  withPostgreSQL connInfo initTables

  newPool $ defaultPoolConfig 
    seldaOpen
    seldaClose 
    connLife 
    connMax 
  where
    connInfo = PGConnectionString
      { pgConnectionString = connStr
      , pgSchema = Nothing
      }

    seldaOpen = pgOpen connInfo

initTables :: MonadSelda m => m ()
initTables = do
  tryCreateTable usersTable

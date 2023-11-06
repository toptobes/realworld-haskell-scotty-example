{-# LANGUAGE TypeFamilies #-}

module Conduit.App.Monad where

import Conduit.App.Env (Env)
import Conduit.App.Has (grab)
import Conduit.DB (DBPool)
import Data.Pool (withResource)
import Database.Selda
import Database.Selda.Backend
import Database.Selda.PostgreSQL (PG)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ActionT)

newtype AppM a = AppM
  { runAppM :: ReaderT Env IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadUnliftIO)

class (Monad m) => MonadApp m where
  liftApp :: AppM a -> m a

instance MonadApp AppM where
  liftApp :: AppM a -> AppM a
  liftApp = id

instance MonadApp (ActionT AppM) where
  liftApp :: AppM a -> ActionT AppM a
  liftApp = lift

instance MonadSelda AppM where
  type Backend AppM = PG

  withConnection :: (SeldaConnection PG -> AppM a) -> AppM a
  withConnection action = do
    pool <- grab @DBPool
    let runAction = withResource pool $ pure . action
    join $ liftIO runAction

{-# LANGUAGE TypeFamilies #-}

module Conduit.App.Monad where

import Conduit.App.Env (Env)
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

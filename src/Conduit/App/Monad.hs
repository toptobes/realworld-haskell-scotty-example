{-# LANGUAGE TypeFamilies #-}

module Conduit.App.Monad where

import Conduit.App.Env (Env)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ActionT)
import Conduit.Errors (FeatureError, handleFeatureErrors)

-- | The app monad. woo.
newtype AppM a = AppM
  { runAppM :: ReaderT Env IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadUnliftIO)

-- | Lifts computations to the app monad level. @(MonadApp m)@ shouldn't be used directly as a constraint;
--   Rather, just use @liftApp@ and see 'Conduit.App.Has.Has'.
class (Monad m) => MonadApp m where
  liftApp :: AppM a -> m a

instance MonadApp AppM where
  liftApp :: AppM a -> AppM a
  liftApp = id
  {-# INLINE liftApp #-}

instance MonadApp (ActionT AppM) where
  liftApp :: AppM a -> ActionT AppM a
  liftApp = lift
  {-# INLINE liftApp #-}

runService :: (FeatureError e) => AppM (Either e a) -> ActionT AppM a
runService = liftApp >=> either handleFeatureErrors pure

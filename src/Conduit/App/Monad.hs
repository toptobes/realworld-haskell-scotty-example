{-# LANGUAGE FieldSelectors #-}

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

-- | Runs a service in the App monad and converts its potentially failed result to the appropriate error if neccesary.
-- 
-- > data MyErr = Aw Text
-- > data MyResult = Yay Text deriving (Generic, ToJSON)
-- > 
-- > instance FeatureError MyErr where
-- >   handleFeatureError (ResultErr msg) = do 
-- >     status status500
-- >     text msg
-- > 
-- > endpoint = get "/" $ do
-- >   (result :: MyResult) <- runService (someAppService :: AppM (Either MyErr MyResult))
-- >   json result
runService :: (FeatureError e) => AppM (Either e a) -> ActionT AppM a
runService = liftApp >=> either handleFeatureErrors pure

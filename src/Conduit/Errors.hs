module Conduit.Errors where

import {-# SOURCE #-} Conduit.DB.Core (DBError)
import Conduit.Utils ((>->))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ActionT, finish)

class FeatureError e where
  -- | Converts a feature error into some scotty response
  handleFeatureError :: (MonadIO m) => e -> ActionT m ()
  -- | Converts a DBError into some feature error
  handleDBError :: DBError -> e

-- | Converts a potentially failed service's result to either the appropriate error or successful request.
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
-- >   (result :: Either MyErr MyResult) <- someService
-- >   withFeatureErrorsHandled result $ \res ->
-- >     json res
withFeatureErrorsHandled :: (MonadIO m, FeatureError e) => Either e a -> (a -> ActionT m ()) -> ActionT m ()
withFeatureErrorsHandled (Left  e) _ = handleFeatureError e
withFeatureErrorsHandled (Right e) action = action e

handleFeatureErrors :: (MonadUnliftIO m, FeatureError e) => e -> ActionT m a
handleFeatureErrors = handleFeatureError >-> finish

-- | Provides an easy way for errors to translate between features to facilitate cross-feature code sharing
class FeatureErrorMapper e1 e2 where
  mapFeatureError :: e1 -> e2

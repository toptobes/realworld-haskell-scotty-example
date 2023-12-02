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

-- | Converts a feature error to its appropriate error response. See 'Conduit.App.Monad.runService'.
handleFeatureErrors :: (MonadUnliftIO m, FeatureError e) => e -> ActionT m a
handleFeatureErrors = handleFeatureError >-> finish

-- | Provides an easy way for errors to translate between features to facilitate cross-feature code sharing
class FeatureErrorMapper e1 e2 where
  mapFeatureError :: e1 -> e2

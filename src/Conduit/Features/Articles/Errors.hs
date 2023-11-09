module Conduit.Features.Articles.Errors where

import Conduit.DB (DBError)
import Conduit.Errors (FeatureErrorHandler(..))
import Web.Scotty.Trans (ActionT)

data ArticleError

instance FeatureErrorHandler ArticleError where
  withFeatureErrorsHandled :: (MonadIO m) => Either ArticleError a -> (a -> ActionT m ()) -> ActionT m ()
  withFeatureErrorsHandled = undefined

  handleDBError :: DBError -> ArticleError
  handleDBError = undefined

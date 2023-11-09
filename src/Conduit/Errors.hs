{-# LANGUAGE AllowAmbiguousTypes #-}

module Conduit.Errors where

import Web.Scotty.Trans (ActionT)
import Conduit.DB (DBError)

class FeatureErrorHandler e where
  withFeatureErrorsHandled :: (MonadIO m) => Either e a -> (a -> ActionT m ()) -> ActionT m ()
  handleDBError :: DBError -> e

mapDBResult :: (FeatureErrorHandler e) => (a -> b) -> Either DBError a -> Either e b
mapDBResult = bimap handleDBError

mapMaybeDBResult :: (FeatureErrorHandler e) => e -> (a -> b) -> Either DBError (Maybe a) -> Either e b
mapMaybeDBResult err f dbResult = do
  result <- handleDBError `first` dbResult
  f <$> maybeToRight err result

mapDBError :: (FeatureErrorHandler e) => Either DBError a -> Either e a
mapDBError = mapDBResult id

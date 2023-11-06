module Conduit.App.Has where

class Has field container where
  obtain :: container -> field

grab :: forall field container m. (MonadReader container m, Has field container) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}

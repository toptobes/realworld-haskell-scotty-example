module Conduit.App.Has where

-- row polymorphism my beloved
class Has field container where
  obtain :: container -> field

grab :: ∀ field container m. (MonadReader container m, Has field container) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}

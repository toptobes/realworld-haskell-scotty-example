module Conduit.App.Has where

-- | Enables more semantic typing & allows for stronger free theorems. sm0rt. See 'grab'.
type Has f c m = (MonadReader c m, Has' f c)

-- | row polymorphism my beloved
class Has' field container where
  obtain :: container -> field

-- | Works with 'Has'
-- 
-- > newtype Name = Name Text deriving (Show)
-- > newtype NameBox = NameBox { nm :: Name }
-- >
-- > instance Has' Name NameBox where 
-- >  obtain :: NameBox -> Name
-- >  obtain = nm
-- >
-- > test :: (Has Name c m, MonadIO m) -> m ()
-- > test = do
-- >   name <- grab @Name
-- >   print name
-- >
-- > main :: IO ()
-- > main = runReaderT test (NameBox $ Name "hi")
grab :: ∀ field container m. (Has field container m) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}

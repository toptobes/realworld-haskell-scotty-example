module Conduit.Utils where

import Data.Aeson (FromJSON(..), Key, ToJSON(..), Value, object, withObject, (.=))
import Data.Aeson.KeyMap (toAscList)
import Data.Aeson.Types (Parser)

-- | A flipped (.) for L->R function composition.
(.-) :: (a -> b) -> (b -> c) -> (a -> c)
f .- g = g . f
{-# INLINE (.-) #-}

-- | Like 'uncurry', but for ternary functions.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 fn (a, b, c) = fn a b c
{-# INLINE uncurry3 #-}

-- | Like L->R Kleisli composition, but ignores result of the first operation.
--
-- > panic :: Text -> IO a
-- > -- panic msg = putTextLn msg >>= const exitFailure
-- > panic = putTextLn >-> exitFailure
infixl 5 >->
(>->) :: (Monad m) => (a -> m b) -> m c -> a -> m c
a >-> b = a >=> const b
{-# INLINE (>->) #-}

-- | An easy method of generating a containing JSON object as the API Spec [often requires](https://realworld-docs.netlify.app/docs/specs/backend-specs/api-response-format).
-- 
-- > inObj "key" 123
--  
--   generates
-- 
-- > { "key": 123 }
data InObj obj = InObj
  { objName   :: !Key
  , objItself :: !obj
  } deriving (Show)

instance (ToJSON obj) => ToJSON (InObj obj) where
  toJSON :: InObj obj -> Value
  toJSON InObj {..} =
    object [objName .= objItself]

instance (FromJSON obj) => FromJSON (InObj obj) where
  parseJSON :: Value -> Parser (InObj obj)
  parseJSON = withObject "InObj" $ \v -> do
    case toAscList v of
      [(key, value)] -> do
        obj <- parseJSON value
        pure $ InObj key obj
      _ -> fail "Expected a single key-value pair"

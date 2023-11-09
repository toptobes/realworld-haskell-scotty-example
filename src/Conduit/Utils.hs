module Conduit.Utils where

import Data.Aeson.KeyMap (toAscList)
import Data.Aeson.Types (FromJSON(..), Key, Parser, ToJSON (..), Value, object, withObject, (.=))

(-.) :: (a -> b) -> (b -> c) -> (a -> c)
f -. g = g . f
{-# INLINE (-.) #-}

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 fn (a, b, c) = fn a b c
{-# INLINE uncurry3 #-}

data InObj obj = InObj
  { objName   :: Key
  , objItself :: obj
  }

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

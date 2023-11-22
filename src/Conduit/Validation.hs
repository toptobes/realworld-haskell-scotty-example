module Conduit.Validation
  ( InObj(InObj)
  , Validations
  , ValErrs(..)
  , are, is
  , notBlank
  , fromJsonObj
  , inErrMsgObj
  ) where

import Data.Aeson.KeyMap (toAscList)
import Data.Aeson.Types (FromJSON(..), Key, Parser, ToJSON(..), Value(..), object, withObject, (.=))
import Data.Map.Strict (fromAscList)
import Data.Text qualified as T
import Network.HTTP.Types (Status (..), status422)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ActionT, StatusError(..), finish, json, jsonData, raiseStatus, rescue, status)

-- | Creates an 'InObj' with the key "message"
inErrMsgObj :: obj -> InObj obj
inErrMsgObj = InObj "message"

-- | An easy method of generating a containing JSON object as the API Spec [often requires](https://realworld-docs.netlify.app/docs/specs/backend-specs/api-response-format).
-- 
-- > inObj "key" 123
--  
--   generates
-- 
-- > { "key": 123 }
data InObj obj = InObj
  { objName   :: Key
  , objItself :: obj
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

-- | Defines the key-error pairs to later be serialized into the [expected format](https://realworld-docs.netlify.app/docs/specs/backend-specs/error-handling).
newtype ValErrs = ValErrs [(Text, Text)]

instance ToJSON ValErrs where
  toJSON :: ValErrs -> Value
  toJSON (ValErrs errs) = 
    object ["errors" .= fromAscList (second (:[]) <$> errs)]

-- | Some function that inputs some value and its associated key and returns the validation errors, if any
type Validator a = a -> Text -> Maybe ValErrs

-- | Some function which describes the desired validations for a given object
-- 
-- > data Example = Example { field :: Text }
-- >
-- > validations :: Validations Example
-- > validations Example {..} = 
-- >   [ (field, "field") `is` notBlank
-- >   ]
type Validations a = a -> [Maybe ValErrs]

-- | applies a validation for a single field
-- 
-- > [(a, "a") `is` notBlank]
infixl 4 `is`
is :: (a, Text) -> Validator a -> Maybe ValErrs
is = flip uncurry
{-# INLINE is #-}

-- | like 'is', but a shorthand to apply a validation to an entire list
-- 
-- > [(a, "a"), (b, "b")] `are` notBlank
infixl 4 `are`
are :: [(a, Text)] -> Validator a -> [Maybe ValErrs]
are a v = map (uncurry v) a
{-# INLINE are #-}

-- | Simple 'Validator' that dicates that a given field have a length of > 0
notBlank :: Validator Text
notBlank txt key = guard (T.null txt) $> ValErrs [(key, "can't be blank")]

validateAll :: a -> Validations a -> Either ValErrs a
validateAll a validations = maybeToLeft a $ asum (validations a)

-- | Deserializes and validates the contents of some encapsulated json object (as it's [very prevalent](https://realworld-docs.netlify.app/docs/specs/backend-specs/endpoints) in the request specs).
--   Returns the [appropriate errors](https://realworld-docs.netlify.app/docs/specs/backend-specs/error-handling) on failues.
-- 
-- > data Example = Example { field :: Text }
-- >   deriving (Show)
-- > 
-- > -- Given: { "irrelevant-key": { "field": "hi!" }}
-- > endpoint = post "/" $ do
-- >   action <- fromJsonObj $ \e -> [(e.field, "field") `is` notBlank]
-- >   print action -- Example { field = "hi!" }
-- 
--   I am aware that this now allows any old key to be in place of "irrelevant-key", but-
fromJsonObj :: ∀ a m. (MonadUnliftIO m, FromJSON a) => Validations a -> ActionT m a
fromJsonObj validations = do
  (InObj _ decoded) <- jsonData `rescue` tryExtractKey

  case validateAll decoded validations of
    Left err -> do
      status status422
      json err
      finish
    Right decoded' -> do
      pure decoded'

-- Pretty hacky but the lack of easily processable errors is super annoying
-- catches if a required key is missing and returns the appropriate err msg
tryExtractKey :: (MonadIO m) => StatusError -> ActionT m a
tryExtractKey (StatusError (Status 422 _) (toStrict -> str)) = do
  let key = T.splitOn "failed, key \"" str !!? 1 <&> T.takeWhile (/= '"')
  whenJust key \k -> json $ ValErrs [(k, "can't be blank")]
  status status422
  finish
tryExtractKey (StatusError sts msg) = raiseStatus sts msg

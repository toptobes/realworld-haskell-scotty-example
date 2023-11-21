module Conduit.Validation
  ( InObj(..)
  , Validations
  , inErrMsgObj
  , fromJsonObj
  , mkErrObj
  , notBlank
  , are, is
  ) where

import Data.Aeson.KeyMap (toAscList)
import Data.Aeson.Types (FromJSON(..), Key, Parser, ToJSON(..), Value(..), object, withObject, (.=))
import Data.Map.Strict (fromAscList)
import Data.Text qualified as T
import Network.HTTP.Types (Status (..), status422)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ActionT, StatusError(..), finish, json, jsonData, raiseStatus, rescue, status)

inErrObj :: obj -> InObj obj
inErrObj = InObj "errors"

inErrMsgObj :: obj -> InObj obj
inErrMsgObj = InObj "message"

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

type ValidatorErr = (Text, Text)
type Validator a = a -> Text -> Maybe ValidatorErr
type Validations a = a -> [Maybe ValidatorErr]

infixl 4 `is`
is :: (a, Text) -> Validator a -> Maybe ValidatorErr
is = flip uncurry

infixl 4 `are`
are :: [(a, Text)] -> Validator a -> [Maybe ValidatorErr]
are a v = map (uncurry v) a

notBlank :: Validator Text
notBlank txt key = guard (T.null txt) $> (key, "can't be blank")

validateAll :: a -> Validations a -> Either (Text, Text) a
validateAll a validations = maybeToLeft a $ asum (validations a)

fromJsonObj :: ∀ a m. (MonadUnliftIO m, FromJSON a) => Validations a -> ActionT m a
fromJsonObj validations = do
  (InObj _ decoded) <- jsonData `rescue` tryExtractKey

  case validateAll decoded validations of
    Left err -> do
      status status422
      json $ mkErrObj [err]
      finish
    Right decoded' -> do
      pure decoded'

mkErrObj :: [ValidatorErr] -> InObj (Map Text [Text])
mkErrObj errs = inErrObj $ fromAscList $ second (:[]) <$> errs

-- Pretty hacky but the lack of easily processable errors is super annoying
tryExtractKey :: (MonadIO m) => StatusError -> ActionT m a
tryExtractKey (StatusError (Status 422 _) (toStrict -> str)) = do
  let key = T.splitOn "failed, key \"" str !!? 1 <&> T.takeWhile (/= '"')
  whenJust key \k -> json $ mkErrObj [(k, "can't be blank")]
  status status422
  finish
tryExtractKey (StatusError sts msg) = raiseStatus sts msg

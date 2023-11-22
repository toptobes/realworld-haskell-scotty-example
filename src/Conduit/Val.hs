{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}

-- module Conduit.Val 
--   ( InObj(..)
--   , NotBlank(..)
--   , IsRed(..)
--   , fromJsonObj
--   , inErrMsgObj
--   , ensureIfExists
--   , ensure
--   ) where

module Conduit.Val where

import Conduit.Utils (InObj(..), (.-))
import Data.Aeson (FromJSON(..), ToJSON(..), Value, eitherDecode, object, (.=))
import Data.Aeson.Types (Parser)
import Data.Map.Strict (fromAscList)
import Data.Text qualified as T
import Network.HTTP.Types (status422)
import Relude.Unsafe as Unsafe ((!!))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ActionT, body, finish, json, status)

inErrMsgObj :: obj -> InObj obj
inErrMsgObj = InObj "message"

newtype Ensure property a = Ensure { getEnsure :: a}
  deriving Show

class Validation property on where
  validate :: on -> Bool
  errMsg :: String

data NotBlank = NotBlank

instance Validation NotBlank Text where
  validate = not . T.null
  errMsg = "can't be blank"

instance (Validation prop on) => Validation prop (Ensure other on) where
  validate = getEnsure .- validate @prop
  errMsg = errMsg @prop @on

-- data IsRed = IsRed

-- instance Validation IsRed Text where
--   validate = (== "red")
--   errMsg = "must be red"

-- newtype Test = Test Text
--   deriving (Show)

-- instance FromJSON Test where
--   parseJSON = withObject "Test" $ \v -> Test
--     <$> v .: "test" <!< IsRed <!< NotBlank

-- eitherDecode @Test "{ \"test\": \"\" }"
-- Left "Error in $.test: can't be blank"

-- eitherDecode @Test "{ \"test\": \"abc\" }"
-- Left "Error in $.test: must be red"

instance (Validation prop on, FromJSON on) => FromJSON (Ensure prop on) where
  parseJSON v = do
    val <- parseJSON v
    if validate @prop val
      then pure $ Ensure val 
      else fail $ errMsg @prop @on

(<?!<) :: Parser (Maybe (Ensure prop on)) -> prop -> Parser (Maybe on)
p <?!< _ = fmap getEnsure <$> p

(<!<) :: Parser (Ensure prop on) -> prop -> Parser on
p <!< _ = getEnsure <$> p

newtype ValErrs = ValErrs [(Text, Text)]
  deriving newtype (Show)

instance ToJSON ValErrs where
  toJSON :: ValErrs -> Value
  toJSON (ValErrs errs) = 
    object ["errors" .= fromAscList (second (:[]) <$> errs)]

fromJsonObj :: ∀ a m. (MonadUnliftIO m, FromJSON a) => ActionT m a
fromJsonObj = body <&> eitherDecode >>= \case
  Left msg -> do
    status status422
    json $ msg2err msg
    finish
  Right (InObj _ a) -> do
    pure a

msg2err :: String -> ValErrs
msg2err (toText -> txt) = ValErrs [go $ T.splitOn ": " txt] where 
  go [path, err] = if T.last path == '$'
    then (T.splitOn "key \"" err Unsafe.!! 1 & T.takeWhile (/= '"'), "can't be blank")
    else (T.splitOn "Error in $." path Unsafe.!! 1, err)
  go split = error $ show split

{-# LANGUAGE UndecidableInstances #-}

module Conduit.Identity.Password
  ( HashedPassword(..)
  , UnsafePassword(..)
  , PasswordGen(..)
  , hashPasswordWithSalt
  ) where

import Crypto.Error (throwCryptoErrorIO)
import Crypto.KDF.Argon2 (Options (..), Variant (..), defaultOptions)
import Crypto.KDF.Argon2 qualified as Argon
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Aeson (FromJSON)
import Data.ByteArray (Bytes, convert)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Text (splitOn)
import Relude.Unsafe as Unsafe
import UnliftIO (stringException)
import UnliftIO.Exception (throwIO)

newtype HashedPassword = HashedPassword { getHashedPassword :: Text }
  deriving newtype (Eq)

newtype UnsafePassword = UnsafePassword { getUnsafePassword :: Text }
  deriving newtype (Read, FromJSON)

class (Monad m) => PasswordGen m where
  hashPassword :: UnsafePassword -> m HashedPassword
  testPassword :: UnsafePassword -> HashedPassword -> m Bool

instance (Monad m, MonadIO m) => PasswordGen m where
  hashPassword :: UnsafePassword -> m HashedPassword
  hashPassword = hashPassword'

  testPassword :: UnsafePassword -> HashedPassword -> m Bool
  testPassword = testPassword'
  
argonOptions :: Options
argonOptions = defaultOptions
  { variant = Argon2id
  , parallelism = 2
  , iterations  = 2
  , memory = 65536 
  }

newSalt :: (MonadIO m) => m ByteString
newSalt = liftIO $ getRandomBytes 16

extractSalt :: (MonadIO m) => HashedPassword -> m ByteString
extractSalt (HashedPassword hash') = case decodeBase64 $ encodeUtf8 $ splitOn "$" hash' Unsafe.!! 4 of
  Left err -> liftIO . throwIO . stringException $ toString err
  Right salt -> pure salt

text2bytes :: Text -> Bytes
text2bytes = convert . encodeUtf8 @_ @ByteString

hashPassword' :: (MonadIO m) => UnsafePassword -> m HashedPassword
hashPassword' unsafe = newSalt >>= hashPasswordWithSalt unsafe

hashPasswordWithSalt :: (MonadIO m) => UnsafePassword -> ByteString -> m HashedPassword
hashPasswordWithSalt (UnsafePassword password) salt = do
  digest <- Argon.hash @_ @Bytes @Bytes argonOptions (text2bytes password) (convert salt) 32 & liftIO . throwCryptoErrorIO
  pure $ mkHashedPassword (convert digest) salt

mkHashedPassword :: ByteString -> ByteString -> HashedPassword
mkHashedPassword digest salt = HashedPassword $ "$argon2id$v=13$m=65536,t=2,p=2$" <> salt' <> "$" <> digest'
  where digest' = encodeBase64 digest; salt' = encodeBase64 salt;

testPassword' :: (MonadIO m) => UnsafePassword -> HashedPassword -> m Bool
testPassword' password hashed = do
  salt <- extractSalt hashed
  hashed' <- hashPasswordWithSalt password salt
  pure $ hashed == hashed' 

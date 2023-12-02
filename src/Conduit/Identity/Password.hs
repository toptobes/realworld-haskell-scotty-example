{-# LANGUAGE UndecidableInstances, FieldSelectors #-}

module Conduit.Identity.Password
  ( HashedPassword(..)
  , UnsafePassword(..)
  , PasswordGen(..)
  , testPassword
  ) where

import Conduit.Utils ((.-))
import Conduit.Validation (Validation(..), NotBlank)
import Crypto.Error (CryptoFailable(..))
import Crypto.KDF.Argon2 (Options(..), Variant(..), defaultOptions)
import Crypto.KDF.Argon2 qualified as Argon
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Aeson (FromJSON)
import Data.ByteArray (Bytes, convert)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Text (splitOn)
import Relude.Unsafe as Unsafe ((!!))

-- | A properly hashed password
newtype HashedPassword = HashedPassword { getHashed :: Text }
  deriving newtype (Eq)

-- | An unsafe plaintext password
newtype UnsafePassword = UnsafePassword { getUnsafe :: Text }
  deriving newtype (FromJSON)

instance Validation NotBlank UnsafePassword where
  validate = validate @NotBlank . getUnsafe
  errMsg = errMsg @NotBlank @Text

-- | Some monad which can properly hash an 'UnsafePassword'
class (Monad m) => PasswordGen m where
  hashPassword :: UnsafePassword -> m HashedPassword

instance (Monad m, MonadIO m) => PasswordGen m where
  hashPassword :: UnsafePassword -> m HashedPassword
  hashPassword = hashPasswordWithSalt .- (<$> newSalt)

-- | Options for Cryptonite's Argon2 algo; strikes a fair balance of performance and security (I think).
argonOptions :: Options
argonOptions = defaultOptions
  { variant = Argon2id
  , parallelism = 2
  , iterations  = 2
  , memory = 65536 
  }

hashStrParams :: Text
hashStrParams = "$argon2id$v=13$m=" <> show m <> ",t=" <> show t <> ",p=" <> show p <> "$"
  where a = argonOptions; m = a.memory; t = a.iterations; p = a.parallelism;

newSalt :: (MonadIO m) => m ByteString
newSalt = liftIO $ getRandomBytes 16

extractSalt :: HashedPassword -> Maybe ByteString
extractSalt (HashedPassword hash') = rightToMaybe . decodeBase64 . encodeUtf8 $ splitOn "$" hash' Unsafe.!! 4

text2bytes :: Text -> Bytes
text2bytes = convert . encodeUtf8 @_ @ByteString

hashPasswordWithSalt :: UnsafePassword -> ByteString -> HashedPassword
hashPasswordWithSalt (UnsafePassword password) salt =
  let digest = Argon.hash @_ @Bytes @Bytes argonOptions (text2bytes password) (convert salt) 32 & \case
        CryptoPassed digest' -> digest'
        CryptoFailed err -> error $ show err -- I think that all of the CryptoErrors are deeper rooted issues and should just fail-fast
   in mkHashedPassword (convert digest) salt

mkHashedPassword :: ByteString -> ByteString -> HashedPassword
mkHashedPassword digest salt = HashedPassword $ hashStrParams <> salt' <> "$" <> digest'
  where digest' = encodeBase64 digest; salt' = encodeBase64 salt;

-- | Validates a plaintext password against its hashed potential counterpart.
testPassword :: UnsafePassword -> HashedPassword -> Bool
testPassword password hashed = do
  let salt = extractSalt hashed
   in maybe False (hashPasswordWithSalt password .- (== hashed)) salt

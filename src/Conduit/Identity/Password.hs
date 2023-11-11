{-# LANGUAGE UndecidableInstances #-}

module Conduit.Identity.Password
  ( HashedPassword(..)
  , UnsafePassword(..)
  , PasswordGen(..)
  , testPassword
  ) where

import Crypto.Error (CryptoFailable(..))
import Crypto.KDF.Argon2 (Options(..), Variant(..), defaultOptions)
import Crypto.KDF.Argon2 qualified as Argon
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Aeson (FromJSON)
import Data.ByteArray (Bytes, convert)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Text (splitOn)
import Relude.Unsafe as Unsafe

{-
I'm tempted to switch to an approach like so, but I'm sure all the other haskellers would frown upon me for not making illegal states
as impossible as possible :(

```
  data Password
    = HashedPassword Text
    | UnsafePassword Text
    
  getHashed (HashedPassword hash) = hash
  getHashed _ = error "Attempt to unwrap a hashed password" -- failfast since this should never happen in properly constructured system
```
-}

newtype HashedPassword = HashedPassword { getHashed :: Text }
  deriving newtype (Eq)

newtype UnsafePassword = UnsafePassword { getUnsafe :: Text }
  deriving newtype (FromJSON)

class (Monad m) => PasswordGen m where
  hashPassword :: UnsafePassword -> m HashedPassword

instance (Monad m, MonadIO m) => PasswordGen m where
  hashPassword :: UnsafePassword -> m HashedPassword
  hashPassword = hashPassword'

argonOptions :: Options
argonOptions = defaultOptions
  { variant = Argon2id
  , parallelism = 2
  , iterations  = 2
  , memory = 65536 
  }

newSalt :: (MonadIO m) => m ByteString
newSalt = liftIO $ getRandomBytes 16

extractSalt :: HashedPassword -> Maybe ByteString
extractSalt (HashedPassword hash') = rightToMaybe . decodeBase64 . encodeUtf8 $ splitOn "$" hash' Unsafe.!! 4

text2bytes :: Text -> Bytes
text2bytes = convert . encodeUtf8 @_ @ByteString

hashPassword' :: (MonadIO m) => UnsafePassword -> m HashedPassword
hashPassword' unsafe = newSalt <&> hashPasswordWithSalt unsafe

hashPasswordWithSalt :: UnsafePassword -> ByteString -> HashedPassword
hashPasswordWithSalt (UnsafePassword password) salt =
  let digest = Argon.hash @_ @Bytes @Bytes argonOptions (text2bytes password) (convert salt) 32 & \case
        CryptoPassed digest' -> digest'
        CryptoFailed err -> error $ show err -- I believe that all of the CryptoErrors are deeper rooted issues and should fail-fast
   in mkHashedPassword (convert digest) salt

mkHashedPassword :: ByteString -> ByteString -> HashedPassword
mkHashedPassword digest salt = HashedPassword $ "$argon2id$v=13$m=65536,t=2,p=2$" <> salt' <> "$" <> digest'
  where digest' = encodeBase64 digest; salt' = encodeBase64 salt;

testPassword :: UnsafePassword -> HashedPassword -> Bool
testPassword password hashed = do
  let salt = extractSalt hashed
   in maybe False ((hashed ==) . hashPasswordWithSalt password) salt

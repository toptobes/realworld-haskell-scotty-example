module Main where

import Conduit
import Data.Aeson

-- Avoiding orphan instances
newtype C = C { unC :: ConduitOps }
newtype P = P { unP :: PGConnOps  }
newtype E = E { unE :: EnvType }
newtype J = J { unJ :: JWTOps  }

instance FromJSON C where
  parseJSON = withObject "C" \v -> fmap C $ ConduitOps
    <$>  v .:? "port" .!= 3000
    <*> (v .:  "pg"   <&> unP)
    <*> (v .:  "jwt"  <&> unJ)
    <*> (v .:  "env"  <&> unE)

instance FromJSON P where
  parseJSON = withObject "P" \v -> fmap P $ PGConnOps
    <$> v .:  "connStr"
    <*> v .:  "poolSize"
    <*> v .:  "maxIdleTime"
    <*> v .:  "numStripes"
    <*> v .:? "truncTables" .!= False

instance FromJSON J where
  parseJSON = withObject "J" \v -> fmap J $ JWTOps
    <$> v .: "secret"
    <*> v .: "ttl"

instance FromJSON E where
  parseJSON = withObject "E" \v -> E
    <$> v .: "type"

mkConduitOps :: LByteString -> IO ConduitOps
mkConduitOps = eitherDecode <&> either (error . toText) (pure . unC)

main :: IO ()
main = lookupEnv "CONDUIT_CONFIG" <&> (?: "conduit.json") 
  >>= readFileLBS 
  >>= mkConduitOps 
  >>= Conduit.main

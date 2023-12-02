module Conduit.App.Env where

import Conduit.App.Has (Has', obtain)
import Conduit.DB.Core (DBPool)
import Conduit.Identity.JWT (JWTInfo)
import Data.Aeson (FromJSON)

-- | The type of the runtime environment. @Development@ has more logging and auto-runs DB migrations.
data EnvType = Production | Development
  deriving (Show, Eq, Read, Generic, FromJSON)

-- | Global state of the application.
data Env = Env
  { envDBPool  :: !DBPool  -- ^ The database connection pool.
  , envJWTInfo :: !JWTInfo -- ^ The necessary info for creating & verifying JWTs.
  , envType    :: !EnvType -- ^ The type of the runtime environment.
  }

instance Has' DBPool Env where
  obtain :: Env -> DBPool
  obtain = (.envDBPool)
  {-# INLINE obtain #-}

instance Has' JWTInfo Env where
  obtain :: Env -> JWTInfo
  obtain = (.envJWTInfo)
  {-# INLINE obtain #-}

instance Has' EnvType Env where
  obtain :: Env -> EnvType
  obtain = (.envType)
  {-# INLINE obtain #-}

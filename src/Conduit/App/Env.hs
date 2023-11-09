module Conduit.App.Env where

import Conduit.App.Has (Has, obtain)
import Conduit.DB (DBPool)
import Conduit.Identity.JWT (JWTInfo)

data EnvType = Production | Development
  deriving (Show, Eq, Read)

data Env = Env
  { envDBPool  :: !DBPool
  , envJWTInfo :: !JWTInfo
  , envType    :: !EnvType
  }

instance Has DBPool Env where
  obtain :: Env -> DBPool
  obtain = envDBPool
  {-# INLINE obtain #-}

instance Has JWTInfo Env where
  obtain :: Env -> JWTInfo
  obtain = envJWTInfo
  {-# INLINE obtain #-}

instance Has EnvType Env where
  obtain :: Env -> EnvType
  obtain = envType
  {-# INLINE obtain #-}

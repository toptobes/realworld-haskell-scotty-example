module Conduit
  ( ConduitOps(..)
  , PGConnOps(..)
  , EnvType(..)
  , JWTOps(..)
  , main
  ) where

import Conduit.App.Env (Env(..), EnvType(..))
import Conduit.App.Monad (runAppM)
import Conduit.DB.Init (PGConnOps(..), initDB, mkDBPool)
import Conduit.Features.Account.Handlers qualified as Account
import Conduit.Features.Articles.Handlers qualified as Articles
import Conduit.Identity.JWT (mkJWTInfo, JWTOps(..))
import Network.HTTP.Types (status500)
import Network.Wai.Middleware.RequestLogger
import Web.Scotty.Trans (Handler (Handler), defaultHandler, middleware, scottyT, status)
import Database.PostgreSQL.Simple (SqlError)
import Network.Wai.Middleware.Static (staticPolicyWithOptions, cacheContainer, CachingStrategy (..), initCaching, addBase, addSlash, hasPrefix, staticPolicy, static)
import Network.Wai.Middleware.Static qualified as Static

data ConduitOps = ConduitOps
  { dbConnOps :: PGConnOps
  , jwtOps    :: JWTOps
  , envType   :: EnvType
  }

main :: ConduitOps -> IO ()
main ConduitOps {..} = do
  env <- defaultEnv
  let runAppToIO m = runReaderT (runAppM m) env

  putStrLn $ fold ["Running in '", show env.envType, "'"]

  when (env.envType == Development) do
    initDB env.envDBPool dbConnOps

  cache <- initCaching PublicStaticCaching

  scottyT 3000 runAppToIO do
    defaultHandler $ Handler \(e :: SqlError) -> do -- temp for debugging reasons
      print e >> status status500
    
    middleware $ loggerFor env.envType

    middleware $ staticPolicyWithOptions (staticOps cache) policy
    
    applicationHandlers
  where
    defaultEnv = Env
      <$> mkDBPool dbConnOps
      <*> pure (mkJWTInfo jwtOps)
      <*> pure envType

    loggerFor = \case
      Development -> logStdoutDev
      _ -> logStdout

    applicationHandlers = fold
      [ Account.handlers
      , Articles.handlers
      ]

    staticOps container = Static.defaultOptions
      { cacheContainer = container
      }

    policy = hasPrefix "images" <> addBase "static"

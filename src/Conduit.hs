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
import Conduit.Identity.JWT (JWTOps(..), mkJWTInfo)
import Database.PostgreSQL.Simple (SqlError)
import Network.HTTP.Types (status500)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static (CachingStrategy(..), addBase, cacheContainer, hasPrefix, initCaching, staticPolicyWithOptions)
import Network.Wai.Middleware.Static qualified as Static
import Web.Scotty.Trans (Handler(..), defaultHandler, middleware, scottyT, status)

data ConduitOps = ConduitOps
  { port      :: Int
  , dbConnOps :: PGConnOps
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

  scottyT port runAppToIO do
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

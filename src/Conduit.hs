module Conduit where

import Conduit.App.Env (Env(..), EnvType(..))
import Conduit.App.Monad (runAppM)
import Conduit.Features.Account.Handlers qualified as Account
import Conduit.Features.Articles.Handlers qualified as Articles
import Conduit.Identity.JWT (Seconds(..), mkJWTInfo)
import Network.Wai.Middleware.RequestLogger
import Relude.Unsafe qualified as Unsafe
import Web.JWT (hmacSecret)
import Web.Scotty.Trans (middleware, scottyT, defaultHandler, status, Handler (Handler))
import Conduit.DB.Init (ConnectionOps(..), mkDBPool, initDB)
import Network.HTTP.Types (status500)

main :: IO ()
main = do
  env <- defaultEnv
  let runAppToIO m = runReaderT (runAppM m) env

  putStrLn $ fold ["Running in '", show env.envType, "'"]

  when (env.envType == Development) do
    initDB env.envDBPool

  scottyT 3000 runAppToIO do
    defaultHandler $ Handler \(e :: SomeException) -> do
      print e >> status status500
    
    middleware $ loggerFor env.envType
    applicationHandlers
  where
    defaultEnv = Env
      <$> mkDBPool getConnOps
      <*> getJWTInfo
      <*> getEnvType

    getEnvType = lookupEnv "RW_ENVTYPE" <&> maybe Development Unsafe.read

    getConnOps = ConnectionOps
      { connStr  = "host=localhost port=5432 user=postgres password=postgres dbname=realworld"
      , connSize = 4
      , connStripes = 1
      , connTimeout = 60
      }

    getJWTInfo = mkJWTInfo
      <$> (lookupEnv "RW_JWT_SECRET" <&> hmacSecret . maybe "mysupersecretkey" toText)
      <*> (lookupEnv "RW_JWT_EXP_TIME" <&> Seconds . maybe (60 * 15) Unsafe.read)

    loggerFor = \case
      Development -> logStdoutDev
      _ -> logStdout

    applicationHandlers = fold
      [ Account.handlers
      , Articles.handlers
      ]

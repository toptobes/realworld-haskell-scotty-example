module Conduit where

import Conduit.App.Env (Env (..), EnvType (..))
import Conduit.App.Monad (runAppM)
import Conduit.DB (ConnectionOps (..), initSelda)
import Conduit.Features.Account qualified as Account
import Conduit.Identity.JWT (Seconds (..), mkJWTInfo)
import Network.Wai.Middleware.RequestLogger
import Relude.Unsafe qualified as Unsafe
import Web.JWT (hmacSecret)
import Web.Scotty.Trans (middleware, scottyT)

main :: IO ()
main = do
  env <- defaultEnv
  let runAppToIO m = runReaderT (runAppM m) env

  putStrLn $ fold ["Running in '", show env.envType, "'"]

  scottyT 3000 runAppToIO do
    middleware $ loggerFor env.envType
    applicationHandlers
  where
    defaultEnv = Env
      <$> initSelda getConnOps
      <*> getJWTInfo
      <*> getEnvType

    getEnvType = lookupEnv "RW_ENVTYPE" <&> maybe Development Unsafe.read

    getConnOps = ConnectionOps
      { connStr  = "postgres://postgres:postgres@localhost:5432/realworld"
      , connLife = 10
      , connMax  = 1
      }

    getJWTInfo = mkJWTInfo
      <$> (lookupEnv "RW_JWT_SECRET" <&> hmacSecret . maybe "mysupersecretkey" toText)
      <*> (lookupEnv "RW_JWT_EXP_TIME" <&> Seconds . maybe (60 * 15) Unsafe.read)

    loggerFor = \case
      Development -> logStdoutDev
      _ -> logStdout

    applicationHandlers = fold
      [ Account.handlers
      ]

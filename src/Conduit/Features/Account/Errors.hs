module Conduit.Features.Account.Errors where

import Network.HTTP.Types (status400, status403, status404, status500)
import Web.Scotty.Trans (ActionT, status)

data AccountError
  = UserNotFoundEx
  | DetailTakenEx
  | UserUnauthorizedEx
  | SomeSQLError

withAccountErrorsHandled :: (MonadIO m) => Either AccountError a -> (a -> ActionT m ()) -> ActionT m ()
withAccountErrorsHandled (Left UserNotFoundEx)     _ = status status404
withAccountErrorsHandled (Left DetailTakenEx)      _ = status status400
withAccountErrorsHandled (Left UserUnauthorizedEx) _ = status status403
withAccountErrorsHandled (Left SomeSQLError)       _ = status status500
withAccountErrorsHandled (Right a) action = action a

module Conduit.Features.Account.Errors where

import Conduit.DB.Errors (DBError(..), FeatureErrorHandler(..))
import Network.HTTP.Types (status400, status403, status404, status500)
import Web.Scotty.Trans (ActionT, status)

data AccountError
  = UserNotFoundEx
  | UsernameTakenEx
  | EmailTakenEx
  | UserUnauthorizedEx
  | SomeDBEx DBError
  deriving (Show, Eq)

instance FeatureErrorHandler AccountError where
  withFeatureErrorsHandled :: (MonadIO m) => Either AccountError a -> (a -> ActionT m ()) -> ActionT m ()
  withFeatureErrorsHandled = withFeatureErrorsHandled'

  handleDBError :: DBError -> AccountError
  handleDBError = handleDBErr'

withFeatureErrorsHandled' :: (MonadIO m) => Either AccountError a -> (a -> ActionT m ()) -> ActionT m ()
withFeatureErrorsHandled' (Left UserNotFoundEx)     _ = status status404
withFeatureErrorsHandled' (Left UsernameTakenEx)    _ = status status400
withFeatureErrorsHandled' (Left EmailTakenEx)       _ = status status400
withFeatureErrorsHandled' (Left UserUnauthorizedEx) _ = status status403
withFeatureErrorsHandled' (Left (SomeDBEx e))       _ = print e >> status status500
withFeatureErrorsHandled' (Right a) action = action a

handleDBErr' :: DBError -> AccountError
handleDBErr' (UniquenessError "username") = UsernameTakenEx
handleDBErr' (UniquenessError "email") = EmailTakenEx
handleDBErr' (UniquenessError _) = error "should never happen; no other uniqueness constraints exist"
handleDBErr' err = SomeDBEx err

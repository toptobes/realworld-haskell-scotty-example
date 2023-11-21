module Conduit.Features.Account.Errors where

import Conduit.DB.Errors (DBError(..), FeatureErrorHandler(..))
import Conduit.Validation (mkErrObj)
import Network.HTTP.Types (status403, status404, status422, status500)
import Web.Scotty.Trans (ActionT, json, status)

data AccountError
  = UserNotFoundEx
  | CredsTaken [Text]
  | BadLoginCredsEx
  | SomeDBEx DBError
  deriving (Show, Eq)

instance FeatureErrorHandler AccountError where
  withFeatureErrorsHandled :: (MonadIO m) => Either AccountError a -> (a -> ActionT m ()) -> ActionT m ()
  withFeatureErrorsHandled = withFeatureErrorsHandled'

  handleDBError :: DBError -> AccountError
  handleDBError = handleDBErr'

withFeatureErrorsHandled' :: (MonadIO m) => Either AccountError a -> (a -> ActionT m ()) -> ActionT m ()
withFeatureErrorsHandled' (Left UserNotFoundEx)   _ = status status404
withFeatureErrorsHandled' (Left (CredsTaken cs))  _ = status status422 >> json (mkErrObj $ cs <&> (,"has already been taken"))
withFeatureErrorsHandled' (Left BadLoginCredsEx)  _ = status status403 >> json (mkErrObj [("email or password", "is invalid")])
withFeatureErrorsHandled' (Left (SomeDBEx e))     _ = print e >> status status500
withFeatureErrorsHandled' (Right a) action = action a

handleDBErr' :: DBError -> AccountError
handleDBErr' (UniquenessError "username") = CredsTaken ["username"]
handleDBErr' (UniquenessError "email") = CredsTaken ["email"]
handleDBErr' (UniquenessError _) = error "should never happen; no other uniqueness constraints exist"
handleDBErr' err = SomeDBEx err

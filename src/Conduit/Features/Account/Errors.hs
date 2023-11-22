module Conduit.Features.Account.Errors where

import Conduit.DB.Errors (DBError(..), FeatureError(..))
import Network.HTTP.Types (status403, status404, status422, status500)
import Web.Scotty.Trans (ActionT, json, status)
import Conduit.Validation (ValErrs(..))

data AccountError
  = UserNotFoundEx
  | CredsTaken [Text]
  | BadLoginCredsEx
  | SomeDBEx DBError
  deriving (Show, Eq)

instance FeatureError AccountError where
  handleFeatureError = handleFeatureError'
  handleDBError = handleDBErr'

handleFeatureError' :: (MonadIO m) => AccountError -> ActionT m ()
handleFeatureError' UserNotFoundEx  = status status404
handleFeatureError' (CredsTaken cs) = status status422 >> json (ValErrs $ cs <&> (,"has already been taken"))
handleFeatureError' BadLoginCredsEx = status status403 >> json (ValErrs [("email or password", "is invalid")])
handleFeatureError' (SomeDBEx e)    = print e >> status status500

handleDBErr' :: DBError -> AccountError
handleDBErr' (UniquenessError "username") = CredsTaken ["username"]
handleDBErr' (UniquenessError "email") = CredsTaken ["email"]
handleDBErr' (UniquenessError _) = error "should never happen; no other uniqueness constraints exist"
handleDBErr' err = SomeDBEx err

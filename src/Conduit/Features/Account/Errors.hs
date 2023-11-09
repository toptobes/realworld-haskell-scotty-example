module Conduit.Features.Account.Errors where

import Conduit.DB (DBError(..))
import Network.HTTP.Types (status400, status403, status404, status500)
import Web.Scotty.Trans (ActionT, status)

data AccountError
  = UserNotFoundEx
  | UsernameTakenEx
  | EmailTakenEx
  | UserUnauthorizedEx
  | SomeDBEx DBError

withAccountErrorsHandled :: (MonadIO m) => Either AccountError a -> (a -> ActionT m ()) -> ActionT m ()
withAccountErrorsHandled (Left UserNotFoundEx)     _ = status status404
withAccountErrorsHandled (Left UsernameTakenEx)    _ = status status400
withAccountErrorsHandled (Left EmailTakenEx)       _ = status status400
withAccountErrorsHandled (Left UserUnauthorizedEx) _ = status status403
withAccountErrorsHandled (Left (SomeDBEx e))       _ = print e >> status status500
withAccountErrorsHandled (Right a) action = action a

handleAccountErr :: DBError -> AccountError
handleAccountErr (UniquenessError "username") = UsernameTakenEx
handleAccountErr (UniquenessError "email") = EmailTakenEx
handleAccountErr (UniquenessError _) = error "should never happen; no other uniqueness constraints exist"
handleAccountErr err = SomeDBEx err

-- I would put this in DB.hs but I cba to break the dependency cycle...
mapMaybeDBResult :: AccountError -> (a -> b) -> Either DBError (Maybe a) -> Either AccountError b
mapMaybeDBResult err f dbResult = do
  result <- handleAccountErr `first` dbResult
  f <$> maybeToRight err result

mapDBResult :: (a -> b) -> Either DBError a -> Either AccountError b
mapDBResult = bimap handleAccountErr

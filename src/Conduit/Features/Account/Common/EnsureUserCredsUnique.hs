{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.Common.EnsureUserCredsUnique where

import Conduit.DB.Errors (mapDBResult)
import Conduit.DB.Types (MonadDB(..))
import Conduit.DB.Utils (suchThat)
import Conduit.Features.Account.DB (User)
import Conduit.Features.Account.Errors (AccountError(..))
import Database.Esqueleto.Experimental (from, selectOne, table, (==.), exists, val, Value (..))
import UnliftIO (MonadUnliftIO)

type Name = Maybe Text
type Mail = Maybe Text

ensureUserCredsUnique :: (ReadUsers m) => Name -> Mail -> m (Either AccountError ())
ensureUserCredsUnique name email = runExceptT do
  errs <- ExceptT $ findDuplicateCreds name email

  ExceptT . pure $ case errs of
    [] -> pure ()
    cs -> Left $ CredsTaken cs

class (Monad m) => ReadUsers m where
  findDuplicateCreds :: Name -> Mail -> m (Either AccountError [Text])

instance (Monad m, MonadUnliftIO m, MonadDB m) => ReadUsers m where
  findDuplicateCreds :: Name -> Mail -> m (Either AccountError [Text])
  findDuplicateCreds name email = mapDBResult processResult <$> runDB do
    selectOne $ do
      let nameExists = checkExists name \n -> 
            exists $ void $ from (table @User)
              `suchThat` \u -> u.username ==. val n

      let mailExists = checkExists email \m -> 
            exists $ void $ from (table @User)
              `suchThat` \u -> u.email ==. val m

      pure (nameExists, mailExists)
    where
      checkExists = flip $ maybe (val False)

processResult :: Maybe (Value Bool, Value Bool) -> [Text]
processResult (Just (Value nameExists, Value mailExists)) = map snd $ filter fst [(nameExists, "username"), (mailExists, "email")]
processResult Nothing = []

{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Tags.GetTags where

import Prelude hiding (get, on)
import Conduit.App.Monad (AppM, liftApp)
import Conduit.DB.Errors (withFeatureErrorsHandled, mapDBResult)
import Conduit.DB.Types (MonadDB(..))
import Conduit.Features.Articles.DB (Article(..))
import Conduit.Features.Articles.Errors (ArticleError(..))
import Conduit.Features.Articles.Types (inTagsObj)
import Database.Esqueleto.Experimental (Value(..), from, table, select)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, get, json)

handleGetTags :: ScottyT AppM ()
handleGetTags = get "/api/tags" do
  article <- liftApp getAllTags
  withFeatureErrorsHandled article $
    json . inTagsObj

class (Monad m) => AquireTags m where
  getAllTags :: m (Either ArticleError [Text])

instance (Monad m, MonadDB m, MonadUnliftIO m) => AquireTags m where
  getAllTags :: m (Either ArticleError [Text])
  getAllTags = mapDBResult toTags <$> runDB do
    select $ do
      a <- from $ table @Article
      pure a.tags

toTags :: [Value [Text]] -> [Text]
toTags = join . map (\(Value v) -> v)

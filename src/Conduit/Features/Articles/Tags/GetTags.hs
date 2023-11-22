{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Tags.GetTags where

import Prelude hiding (get, on)
import Conduit.App.Monad (AppM, runService)
import Conduit.DB.Core (MonadDB(..), mapDBResult)
import Conduit.Features.Articles.DB (Article(..))
import Conduit.Features.Articles.Errors (ArticleError(..))
import Conduit.Features.Articles.Types (inTagsObj)
import Data.List (nub)
import Database.Esqueleto.Experimental (Value(..), distinct, from, select, table)
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Trans (ScottyT, get, json)

-- I genuinely don't understand what this endpoint is really supposed to do, as the realworld
-- api spec is annoyingly vague. I tried just returning all unique tags from the DB, but there
-- are 18 unique tags when running the cypress test, but the test expects anywhere from [1..10]??
handleGetTags :: ScottyT AppM ()
handleGetTags = get "/api/tags" do
  article <- runService getAllTags
  json $ inTagsObj article

class (Monad m) => AquireTags m where
  getAllTags :: m (Either ArticleError [Text])

instance (Monad m, MonadDB m, MonadUnliftIO m) => AquireTags m where
  getAllTags :: m (Either ArticleError [Text])
  getAllTags = mapDBResult toTags <$> runDB do
    select $ distinct do
      a <- from $ table @Article
      pure a.tags

-- todo: swap out with fixed version so I don't need to do nub here
-- select distinct tag from (select json_array_elements_text(tags::json) as tag from article) as tags;

toTags :: [Value [Text]] -> [Text]
toTags = take 10 . nub . join . map (\(Value v) -> v)

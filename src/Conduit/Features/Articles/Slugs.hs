{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Slugs
  ( extractIDFromSlug
  , mkNoIDSlug
  , mkSlug
  ) where

import Conduit.Features.Articles.Types (ArticleID(..), NoIDSlug(..), Slug(..))
import Data.Char (isAlphaNum, isSpace)
import Data.Text qualified as T
import Conduit.Features.Articles.Errors (ArticleError (InvalidSlugEx))

mkSlug :: ArticleID -> NoIDSlug -> Slug
mkSlug articleID slug = Slug $ show articleID.unID <> "-" <> slug.unSlug

mkNoIDSlug :: Text -> NoIDSlug
mkNoIDSlug = NoIDSlug . T.toLower . T.filter isValidSlugChar . T.map space2dash

space2dash :: Char -> Char
space2dash c
  | isSpace c = '-'
  | otherwise = c

isValidSlugChar :: Char -> Bool
isValidSlugChar c = isAlphaNum c || c == '-'

extractIDFromSlug :: Slug -> Either ArticleError ArticleID
extractIDFromSlug slug = slug.unSlug
   &  T.splitOn "-"
   &  viaNonEmpty head
  <&> toString
  >>= readMaybe
   &  maybeToRight InvalidSlugEx

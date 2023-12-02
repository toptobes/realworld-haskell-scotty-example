{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Articles.Slugs
  ( extractIDFromSlug
  , mkNoIDSlug
  , mkSlug
  ) where

import Conduit.Features.Articles.Errors (ArticleError (..))
import Conduit.Features.Articles.Types (ArticleID(..), NoIDSlug(..), Slug(..))
import Data.Char (isAlphaNum, isSpace)
import Data.Text qualified as T

-- | Creates a Slug where a slug is "<articleID>-<slugifiedTitle>". Depends on a formerly built 'NoIDSlug'.
mkSlug :: ArticleID -> NoIDSlug -> Slug
mkSlug articleID slug = Slug $ show articleID.unID <> "-" <> slug.unSlug

-- | Creates a NoIDSlug where the ID-less slug is the alphanumeric words of the title with the spaces becoming dashes.
--   Note that it's ID-less and only suitable as an intermediate representation.
mkNoIDSlug :: Text -> NoIDSlug
mkNoIDSlug = NoIDSlug . T.toLower . T.filter isValidSlugChar . T.map space2dash

space2dash :: Char -> Char
space2dash c
  | isSpace c = '-'
  | otherwise = c

isValidSlugChar :: Char -> Bool
isValidSlugChar c = isAlphaNum c || c == '-'

-- | Attempts to extract the ID from a Slug, where the slug is "<articleID>-<slugifiedTitle>".
-- 
--   The reason it's an opaque-er newtype rather than a proper invarient is just because
--   'Slug's are allowed to be created without proper validation, with it rather being deferred
--   to this function instead.
extractIDFromSlug :: Slug -> Either ArticleError ArticleID
extractIDFromSlug slug = slug.unSlug
   &  T.splitOn "-"
   &  viaNonEmpty head
  <&> toString
  >>= readMaybe
   &  maybeToRight InvalidSlugEx

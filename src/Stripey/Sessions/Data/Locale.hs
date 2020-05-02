{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Sessions.Data.Locale
  ( Locale (..),
  )
where

import Data.Aeson
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Protolude
import Web.Internal.HttpApiData (ToHttpApiData (..))

data Locale
  = Auto
  | DA
  | DE
  | EN
  | ES
  | FI
  | FR
  | IT
  | JA
  | MS
  | NL
  | NB
  | PL
  | PT
  | SV
  | ZH
  deriving (Show, Generic)

instance FromJSON Locale where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

instance ToJSON Locale where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

instance ToHttpApiData Locale where
  toUrlPiece shipping = T.dropAround (== '"') . TL.toStrict $ TLE.decodeUtf8 (encode shipping)

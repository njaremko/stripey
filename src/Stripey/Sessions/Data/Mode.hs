{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Sessions.Data.Mode
  ( Mode (..),
  )
where

import Data.Aeson
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Protolude
import Web.Internal.HttpApiData (ToHttpApiData (..))

data Mode = Payment | Setup | Subscription deriving (Show, Generic)

instance FromJSON Mode where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

instance ToJSON Mode where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

instance ToHttpApiData Mode where
  toUrlPiece shipping = T.dropAround (== '"') . TL.toStrict $ TLE.decodeUtf8 (encode shipping)

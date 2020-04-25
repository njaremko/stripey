{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Stripey.Sessions.Data.BillingAddressCollection
  ( BillingAddressCollection(..),
  )
where

import Data.Aeson
import Data.Char (toLower)
import Protolude
import Web.Internal.HttpApiData (ToHttpApiData(..))
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

data BillingAddressCollection = Auto | Required deriving (Show, Generic)

instance FromJSON BillingAddressCollection where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

instance ToJSON BillingAddressCollection where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

instance ToHttpApiData BillingAddressCollection where
  toUrlPiece shipping = T.dropAround (=='"') . TL.toStrict $ TLE.decodeUtf8 (encode shipping)
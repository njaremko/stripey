{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Customers.Data.TaxExempt where

import Data.Aeson
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Protolude
import Web.Internal.HttpApiData (ToHttpApiData (..))

data TaxExempt = None | Exempt | Reverse deriving (Generic)

instance FromJSON TaxExempt where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

instance ToJSON TaxExempt where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

instance ToHttpApiData TaxExempt where
  toUrlPiece shipping = T.dropAround (== '"') . TL.toStrict $ TLE.decodeUtf8 (encode shipping)

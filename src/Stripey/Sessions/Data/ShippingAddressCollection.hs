{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Stripey.Sessions.Data.ShippingAddressCollection
  ( ShippingAddressCollection,
  )
where

import Data.Aeson
import Protolude

newtype ShippingAddressCollection = ShippingAddressCollection {
    allowed_countries :: Maybe [Text]
  }
  deriving (Show, Generic)

instance FromJSON ShippingAddressCollection where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ShippingAddressCollection where
  toJSON = genericToJSON defaultOptions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Charges.Data.BillingAddress
  ( BillingAddress,
  )
where

import Data.Aeson
import Protolude

data BillingAddress = BillingAddress
  { city :: Maybe Text,
    country :: Maybe Text,
    line1 :: Maybe Text,
    line2 :: Maybe Text,
    postal_code :: Maybe Text,
    state :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON BillingAddress where
  parseJSON = genericParseJSON defaultOptions

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Stripey.Charges.Data.BillingDetails
  ( BillingDetails,
  )
where

import Data.Aeson
import Stripey.Charges.Data.BillingAddress (BillingAddress)
import Protolude

data BillingDetails
  = BillingDetails
      { address :: BillingAddress,
        email :: Maybe Text,
        name :: Maybe Text,
        phone :: Maybe Text
      }
  deriving (Show, Generic)

instance FromJSON BillingDetails where
  parseJSON = genericParseJSON defaultOptions

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Stripey.Charges.Data.PaymentMethodDetails.Card
  ( Card,
  )
where

import Data.Aeson
import Data.Char (toLower)
import Stripey.Charges.Data.CardBrand
import Stripey.Charges.Data.CardNetwork
import Stripey.Charges.Data.ChargeFunding
import Protolude
data CardInstallments
  = CardInstallments
      { plan :: CardInstallmentsPlan
      }
  deriving (Show, Generic, FromJSON)

data Check = Pass | Fail | Unavailable | Unchecked deriving (Show, Generic)

instance FromJSON Check where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

data CardInstallmentsPlan
  = CardInstallmentsPlan
      { count :: Maybe Int,
        interval :: Maybe Text,
        _type :: Maybe Text
      }
  deriving (Show, Generic, FromJSON)

data CardChecks
  = CardChecks
      { address_line1_check :: Maybe Check,
        address_postal_code :: Maybe Check,
        cvc_check :: Maybe Check
      }
  deriving (Show, Generic, FromJSON)

data Card
  = Card
      { brand :: CardBrand,
        checks :: CardChecks,
        country :: Text,
        exp_month :: Int,
        exp_year :: Int,
        fingerprint :: Text,
        funding :: ChargeFunding,
        installments :: Maybe CardInstallments,
        last4 :: Text,
        network :: CardNetwork
      }
  deriving (Show, Generic, FromJSON)

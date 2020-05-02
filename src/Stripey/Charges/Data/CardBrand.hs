{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Charges.Data.CardBrand
  ( CardBrand,
  )
where

import Data.Aeson
import Data.Char (toLower)
import Data.GenValidity
import Data.GenValidity.Aeson ()
import Data.Validity
import Data.Validity.Aeson ()
import Protolude

data CardBrand = Amex | Diners | Discover | JCB | MasterCard | UnionPay | Visa | Unknown deriving (Show, Generic, Eq)

instance GenValid CardBrand

instance GenUnchecked CardBrand

instance Validity CardBrand

instance FromJSON CardBrand where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

instance ToJSON CardBrand where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Stripey.Charges.Data.CardNetwork
  ( CardNetwork,
  )
where

import Data.Aeson
import Data.Char (toLower)
import Protolude

data CardNetwork = Amex | Diners | Discover | Interac | JCB | MasterCard | UnionPay | Visa | Unknown deriving (Show, Generic)

instance FromJSON CardNetwork where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

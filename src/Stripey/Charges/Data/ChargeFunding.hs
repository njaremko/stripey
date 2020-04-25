{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Stripey.Charges.Data.ChargeFunding
  ( ChargeFunding,
  )
where

import Data.Aeson
import Data.Char (toLower)
import Protolude
data ChargeFunding = Credit | Debit | Prepaid | Unknown deriving (Show, Generic)

instance FromJSON ChargeFunding where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

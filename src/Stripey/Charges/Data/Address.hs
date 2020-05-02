{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Charges.Data.Address
  ( Address (..),
  )
where

import Data.Aeson
import Protolude

data Address = Address
  { city :: Maybe Text,
    country :: Maybe Text,
    line1 :: Maybe Text,
    line2 :: Maybe Text,
    postal_code :: Maybe Text,
    state :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Address where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Address where
  toJSON = genericToJSON defaultOptions

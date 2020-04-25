{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Stripey.Sessions.Data.Shipping
  ( Shipping,
  )
where

import Data.Aeson
import Protolude
import Stripey.Charges.Data.Address

data Shipping = Shipping {
  address :: Address,
  name :: Text
} deriving (Show, Generic)

instance FromJSON Shipping where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Shipping where
  toJSON = genericToJSON defaultOptions
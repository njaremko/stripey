{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Charges.Data.Shipping
  ( Shipping,
  )
where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Protolude
import Stripey.Charges.Data.Address
import Web.HttpApiData (ToHttpApiData (..))

data Shipping = Shipping
  { address :: Address,
    name :: Text,
    carrier :: Maybe Text,
    phone :: Maybe Text,
    tracking_number :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Shipping where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Shipping where
  toJSON = genericToJSON defaultOptions

instance ToHttpApiData Shipping where
  toUrlPiece shipping = T.dropAround (== '"') . TL.toStrict $ TLE.decodeUtf8 (encode shipping)

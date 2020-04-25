{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Stripey.Sessions.Data.LineItem
  ( LineItem(..),
  toStripeParams
  )
where

import Data.Aeson
import Protolude
import Stripey.Charges.Data.Currency
import Network.HTTP.Req ((=:))
import qualified Network.HTTP.Req as Req
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Web.HttpApiData (ToHttpApiData)

data LineItem = LineItem {
  amount :: Int,
  currency :: Currency,
  name :: Text,
  quantity :: Int,
  description :: Maybe Text,
  tax_rates :: Maybe Int
} deriving (Show, Generic)

instance FromJSON LineItem where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON LineItem where
  toJSON = genericToJSON defaultOptions

toStripeParams :: [LineItem] -> Req.Option scheme
toStripeParams items = foldr ((<>) . toParams) mempty (zip [0 ..] items)
  where
    toParams :: (Int, LineItem) -> Req.Option scheme
    toParams (index, val) =
      (("line_items" <> "[" <> T.pack (show index) <> "][name]") =: name val) <>
      (("line_items" <> "[" <> T.pack (show index) <> "][amount]") =: amount val) <>
      (("line_items" <> "[" <> T.pack (show index) <> "][currency]") =: (T.dropAround (=='"') . TL.toStrict . TLE.decodeUtf8 . encode) (currency val)) <>
      (("line_items" <> "[" <> T.pack (show index) <> "][quantity]") =: quantity val) <>
      handleMaybe (index, "description") (description val) <>
      handleMaybe (index, "tax_rates") (tax_rates val)
    handleMaybe :: ToHttpApiData a => (Int, Text) -> Maybe a -> Req.Option scheme
    handleMaybe _ Nothing = mempty
    handleMaybe (index, t) (Just v) = ("line_items" <> "[" <> T.pack (show index) <> "][" <> t <> "]") =: v

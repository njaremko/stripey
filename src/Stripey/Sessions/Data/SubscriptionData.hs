{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Sessions.Data.SubscriptionData
  ( SubscriptionData (..),
    SubscriptionItem (..),
    toStripeParams,
  )
where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Network.HTTP.Req ((=:))
import qualified Network.HTTP.Req as Req
import Protolude
import Stripey.Charges.Data.Address
import Stripey.Sessions.Data.BillingAddressCollection
import Web.HttpApiData (ToHttpApiData)

data SubscriptionItem = SubscriptionItem
  { plan :: Text,
    quantity :: Maybe Int,
    tax_rates :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON SubscriptionItem where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON SubscriptionItem where
  toJSON = genericToJSON defaultOptions

data SubscriptionData = SubscriptionData
  { items :: [SubscriptionItem],
    address :: Address,
    coupon :: Maybe Text,
    default_tax_rates :: Maybe Int,
    trial_end :: Maybe Int,
    trial_from_plan :: Maybe Bool,
    trial_period_days :: Maybe Int,
    billing_address_collection :: Maybe BillingAddressCollection
  }
  deriving (Show, Generic)

instance FromJSON SubscriptionData where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON SubscriptionData where
  toJSON = genericToJSON defaultOptions

toStripeParams :: SubscriptionData -> Req.Option scheme
toStripeParams item =
  toStripeParams' (items item)
    <> handleMaybe "coupon" (coupon item)
    <> handleMaybe "default_tax_rates" (default_tax_rates item)
    <> handleMaybe "trial_end" (trial_end item)
    <> handleMaybe "trial_from_plan" (trial_from_plan item)
    <> handleMaybe "trial_period_days" (trial_period_days item)
  where
    handleMaybe :: ToHttpApiData a => Text -> Maybe a -> Req.Option scheme
    handleMaybe _ Nothing = mempty
    handleMaybe t (Just v) = ("subscription_data[" <> t <> "]") =: v

toStripeParams' :: [SubscriptionItem] -> Req.Option scheme
toStripeParams' sub_items = foldr ((<>) . toParams) mempty (zip [0 ..] sub_items)
  where
    toParams :: (Int, SubscriptionItem) -> Req.Option scheme
    toParams (index, val) =
      (("subscription_data[items]" <> "[" <> T.pack (show index) <> "][plan]") =: plan val)
        <> handleMaybe (index, "quantity") (quantity val)
        <> handleMaybe (index, "tax_rates") (tax_rates val)
    handleMaybe :: ToHttpApiData a => (Int, Text) -> Maybe a -> Req.Option scheme
    handleMaybe _ Nothing = mempty
    handleMaybe (index, t) (Just v) = ("subscription_data[items]" <> "[" <> T.pack (show index) <> "][" <> t <> "]") =: v

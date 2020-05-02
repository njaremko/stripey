{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Sessions.Data.Session
  ( Session,
  )
where

import Data.Aeson
import Protolude hiding (Option)
import Stripey.Sessions.Data.Locale
import Stripey.Sessions.Data.Shipping
import Stripey.Sessions.Data.ShippingAddressCollection
import Stripey.Sessions.Data.SubmitType

data Session = Session
  { id :: Text,
    cancel_url :: Maybe Text,
    client_reference_id :: Maybe Text,
    customer :: Maybe Text,
    customer_email :: Maybe Text,
    object :: Maybe Text,
    billing_address_collection :: Maybe Text,
    mode :: Maybe Text,
    payment_intent :: Maybe Text,
    payment_method_types :: Maybe [Text],
    success_url :: Maybe Text,
    livemode :: Maybe Bool,
    shipping :: Maybe Shipping,
    submit_type :: Maybe SubmitType,
    setup_intent :: Maybe Text,
    subscription :: Maybe Text,
    shipping_address_collection :: Maybe ShippingAddressCollection,
    locale :: Maybe Locale
  }
  deriving (Show, Generic, FromJSON)

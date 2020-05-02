{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Customers.Data.Customer
  ( Customer (..),
  )
where

import Protolude

data Customer = Customer
  { id :: Text,
    -- address ::
    description :: Maybe Text,
    email :: Maybe Text,
    --  metadata :: Maybe Text,
    name :: Maybe Text,
    phone :: Maybe Text,
    --  shipping :: Maybe Text,
    object :: Maybe Text,
    balance :: Maybe Int,
    created :: Maybe Int,
    currency :: Maybe Int,
    data_source :: Maybe Text,
    delinquent :: Maybe Bool,
    --  discount  :: Maybe Bool,
    invoice_prefix :: Maybe Text,
    --  invoice_settings  :: Maybe Text,
    livemode :: Maybe Bool,
    next_invoice_sequence :: Maybe Int,
    preferred_locales :: Maybe [Text],
    --  sources :: Maybe Sources,
    --  sources :: Maybe Sources,
    tax_exempt :: Maybe Text
    -- tax_ids :: Maybe TaxIDs
  }

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Charges.Data.Charge
  ( Charge,
  )
where

import Data.Aeson
import Protolude hiding (Option)
import Stripey.Charges.Data.BillingDetails (BillingDetails)
import Stripey.Charges.Data.Currency (Currency)
import Stripey.Charges.Data.FraudDetails (FraudDetails)
import Stripey.Charges.Data.Outcome (Outcome)
import Stripey.Charges.Data.PaymentMethodDetails (PaymentMethodDetails)

data Charge = Charge
  { id :: Text,
    object :: Text,
    amount :: Int,
    amount_refunded :: Int,
    application :: Maybe Text,
    application_fee :: Maybe Text,
    application_fee_amount :: Maybe Int,
    balance_transaction :: Text,
    billing_details :: BillingDetails,
    calculated_statement_descriptor :: Maybe Text,
    captured :: Bool,
    created :: Int,
    currency :: Currency,
    customer :: Maybe Text,
    description :: Maybe Text,
    disputed :: Bool,
    failure_code :: Maybe Text,
    failure_message :: Maybe Text,
    fraud_details :: Maybe FraudDetails,
    invoice :: Maybe Text,
    livemode :: Bool,
    metadata :: Value,
    on_behalf_of :: Maybe Text,
    order :: Maybe Text,
    outcome :: Maybe Outcome,
    paid :: Bool,
    payment_intent :: Maybe Text,
    payment_method :: Text,
    payment_method_details :: PaymentMethodDetails,
    receipt_email :: Maybe Text,
    receipt_number :: Maybe Text,
    receipt_url :: Maybe Text,
    refunded :: Bool,
    refunds :: Value,
    review :: Maybe Text,
    shipping :: Maybe Value,
    source_transfer :: Maybe Text,
    statement_descriptor :: Maybe Text,
    statement_descriptor_suffix :: Maybe Text,
    status :: Text,
    transfer_data :: Maybe Value,
    transfer_group :: Maybe Text
  }
  deriving (Show, Generic, FromJSON)

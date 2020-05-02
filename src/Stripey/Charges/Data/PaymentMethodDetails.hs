{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Charges.Data.PaymentMethodDetails
  ( PaymentMethodDetails,
    Card,
    ACHCreditTransfer,
  )
where

import Data.Aeson
import Protolude
import Stripey.Charges.Data.PaymentMethodDetails.ACHCreditTransfer (ACHCreditTransfer)
import Stripey.Charges.Data.PaymentMethodDetails.Card (Card)

data PaymentMethodDetails = PaymentMethodDetails
  { ach_credit_transfer :: Maybe ACHCreditTransfer,
    card :: Maybe Card
  }
  deriving (Show, Generic, FromJSON)

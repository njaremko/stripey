{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}

module Stripey.Charges.Data.PaymentMethodDetails
  ( PaymentMethodDetails,
    Card,
    ACHCreditTransfer,
  )
where

import Data.Aeson
import Stripey.Charges.Data.PaymentMethodDetails.ACHCreditTransfer (ACHCreditTransfer)
import Stripey.Charges.Data.PaymentMethodDetails.Card (Card)
import Protolude
data PaymentMethodDetails
  = PaymentMethodDetails
      { ach_credit_transfer :: Maybe ACHCreditTransfer,
        card :: Maybe Card
      }
  deriving (Show, Generic, FromJSON)

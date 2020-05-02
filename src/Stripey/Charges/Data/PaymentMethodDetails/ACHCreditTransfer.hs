{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Charges.Data.PaymentMethodDetails.ACHCreditTransfer
  ( ACHCreditTransfer,
  )
where

import Data.Aeson
import Protolude

data ACHCreditTransfer = ACHCreditTransfer
  { account_number :: Text,
    bank_name :: Text,
    routing_number :: Text,
    swift_code :: Text
  }
  deriving (Show, Generic, FromJSON)

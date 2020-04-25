{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Stripey.Charges.Data.PaymentMethodDetails.ACHCreditTransfer
  ( ACHCreditTransfer,
  )
where
import Protolude
import Data.Aeson

data ACHCreditTransfer
  = ACHCreditTransfer
      { account_number :: Text,
        bank_name :: Text,
        routing_number :: Text,
        swift_code :: Text
      }
  deriving (Show, Generic, FromJSON)

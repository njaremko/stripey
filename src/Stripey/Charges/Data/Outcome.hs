{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Charges.Data.Outcome
  ( Outcome,
  )
where

import Data.Aeson
import Data.Char (toLower)
import Protolude
import Text.Casing (snake)

data OutcomeRiskLevel = Normal | Elevated | Highest | NotAssessed | Unknown deriving (Show, Generic)

instance FromJSON OutcomeRiskLevel where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

data Outcome = Outcome
  { outcome_risk_score :: Int,
    outcome_risk_level :: Text,
    outcome_network_status :: OutcomeNetworkStatus,
    outcome_reason :: Maybe Text,
    outcome_seller_message :: Text,
    outcome_type :: OutcomeType
  }
  deriving (Show, Generic)

instance FromJSON Outcome where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 8}

data OutcomeNetworkStatus = ApprovedByNetwork | DeclinedByNetwork | NotSentToNetwork | ReversedAfterApproval deriving (Show, Generic)

instance FromJSON OutcomeNetworkStatus where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower . snake}

data OutcomeType = Authorized | ManualReview | IssuerDeclined | Blocked | Invalid deriving (Show, Generic)

instance FromJSON OutcomeType where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower . snake}

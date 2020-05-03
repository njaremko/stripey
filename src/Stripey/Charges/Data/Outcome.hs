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
import Control.Carrier.Fail.Either
import Data.Aeson.Types (Parser)

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

parseOutcomeRiskLevel :: Text -> Parser OutcomeNetworkStatus
parseOutcomeRiskLevel "approved_by_network" = return ApprovedByNetwork
parseOutcomeRiskLevel "declined_by_network" = return DeclinedByNetwork
parseOutcomeRiskLevel "not_sent_to_network" = return NotSentToNetwork
parseOutcomeRiskLevel "reversed_after_approval" =  return ReversedAfterApproval
parseOutcomeRiskLevel _ = fail "Invalid outcome risk level!"

instance FromJSON OutcomeNetworkStatus where
  parseJSON = withText "string" parseOutcomeRiskLevel

data OutcomeType = Authorized | ManualReview | IssuerDeclined | Blocked | Invalid deriving (Show, Generic)

parseOutcomeType :: Text -> Parser OutcomeType
parseOutcomeType "authorized" =  return Authorized
parseOutcomeType "manual_review" = return ManualReview
parseOutcomeType "issuer_declined" = return IssuerDeclined
parseOutcomeType "blocked" = return Blocked
parseOutcomeType "invalid" = return Invalid
parseOutcomeType _ = fail "Invalid outcome type!"

instance FromJSON OutcomeType where
  parseJSON = withText "string" parseOutcomeType

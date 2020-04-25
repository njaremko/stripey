{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Stripey.Charges.Data.FraudDetails
  ( FraudDetails,
  )
where

import Data.Aeson
import Protolude
import Web.HttpApiData (ToHttpApiData(..))
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

data FraudDetails
  = FraudDetails
      { stripe_report :: Maybe Text,
        user_report :: Maybe Text
      }
  deriving (Show, Generic)

instance FromJSON FraudDetails where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON FraudDetails where
  toJSON = genericToJSON defaultOptions

instance ToHttpApiData FraudDetails where
  toUrlPiece shipping = T.dropAround (=='"') . TL.toStrict $ TLE.decodeUtf8 (encode shipping)
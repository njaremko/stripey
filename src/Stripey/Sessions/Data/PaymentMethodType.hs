{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Stripey.Sessions.Data.PaymentMethodType
  ( PaymentMethodType(..),
  toStripeParams
  )
where

import Data.Aeson
import Data.Char (toLower)
import Protolude
import Network.HTTP.Req (FormUrlEncodedParam, (=:))
import qualified Data.Text as T

data PaymentMethodType = Card | Ideal | FPX deriving (Show, Generic)

toText :: PaymentMethodType -> Text
toText Card = "card"
toText Ideal = "ideal"
toText FPX = "fpx"

instance FromJSON PaymentMethodType where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

instance ToJSON PaymentMethodType where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

toStripeParams :: [PaymentMethodType] -> FormUrlEncodedParam
toStripeParams types = foldr ((<>) . toStr) mempty (zip [0 ..] types)
  where
    toStr :: (Int, PaymentMethodType) -> FormUrlEncodedParam
    toStr (index, val) = ("payment_method_types" <> "[" <> T.pack (show index) <> "]") =: toText val
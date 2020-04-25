{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}

module Stripey.Charges.Create
  ( createCharge,
    createCharge',
    withDescription,
    withReceiptEmail,
    withStatementDescriptor,
    withStatementDescriptorSuffix,
    withCustomer,
    withSource,
    CreateChargeResponse,
  )
where

import Capability.Reader
import qualified Data.Aeson as Aeson
import Data.Aeson hiding (defaultOptions)
import qualified Data.Text as T
import Network.HTTP.Req
import Stripey.Charges.Data.Charge (Charge)
import Stripey.Charges.Data.Currency (Currency)
import Stripey.Env
import Protolude hiding (Option)

data CreateChargeResponse
  = CreateChargeResponse
      { response_object :: Text,
        response_data :: [Charge],
        response_has_more :: Bool,
        response_url :: Text
      }
  deriving (Show, Generic)

instance FromJSON CreateChargeResponse where
  parseJSON = genericParseJSON Aeson.defaultOptions {fieldLabelModifier = drop 9}

withDescription :: Text -> Option scheme
withDescription c = queryParam "description" (Just c)

withCustomer :: Text -> Option scheme
withCustomer c = queryParam "customer" (Just c)

withSource :: Text -> Option scheme
withSource c = queryParam "source" (Just c)

withReceiptEmail :: Text -> Option scheme
withReceiptEmail c = queryParam "receipt_email" (Just c)

withStatementDescriptor :: Text -> Either Text (Option scheme)
withStatementDescriptor c
  | T.length c > 22 = Left "Statement Descriptor cannot be longer than 22 characters."
  | otherwise = Right (queryParam "statement_descriptor" (Just c))

withStatementDescriptorSuffix :: Text -> Either Text (Option scheme)
withStatementDescriptorSuffix c
  | T.length c > 22 = Left "Statement Descriptor Suffix cannot be longer than 22 characters."
  | otherwise = Right (queryParam "statement_descriptor_suffix" (Just c))

createCharge :: (HasReader "apiToken" ByteString m, MonadHttp m) => Int -> Currency -> (Network.HTTP.Req.Option 'Https -> m Charge)
createCharge a c = mkRequest (createCharge' a c)

createCharge' ::
  MonadHttp m0 =>
  FromJSON a0 =>
  Int ->
  Currency ->
  Network.HTTP.Req.Option 'Https ->
  m0 (JsonResponse a0)
createCharge' a c options =
  req
    POST
    (https "api.stripe.com" /: "v1" /: "charges")
    NoReqBody
    jsonResponse $
    queryParam "amount" (Just a)
    <> queryParam "currency" (Just (T.toLower . T.pack . show $ c))
    <> options

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Stripey.Charges.List
  ( Charge,
    ListChargesResponse,
    listCharges,
    defaultOptions,
    withCustomer,
    withLimit,
    withCreated,
    withEndingBefore,
    withPaymentIntent,
    withStartingAfter,
    withTransferGroup,
  )
where

import Capability.Reader
import qualified Data.Aeson as Aeson
import Data.Aeson hiding (defaultOptions)
import Network.HTTP.Req
import Protolude hiding (MonadReader, Option, ask)
import Stripey.Charges.Data.Charge (Charge)
import Stripey.Env

data ListChargesResponse = ListChargesResponse
  { response_object :: Text,
    response_data :: [Charge],
    response_has_more :: Bool,
    response_url :: Text
  }
  deriving (Show, Generic)

instance FromJSON ListChargesResponse where
  parseJSON = genericParseJSON Aeson.defaultOptions {fieldLabelModifier = drop 9}

withCustomer :: Text -> Option scheme
withCustomer c = queryParam "customer" (Just c)

withCreated :: Int -> Option scheme
withCreated c = queryParam "created" (Just c)

withEndingBefore :: Text -> Option scheme
withEndingBefore c = queryParam "ending_before" (Just c)

withLimit :: Int -> Option scheme
withLimit c = queryParam "limit" (Just c)

withPaymentIntent :: Text -> Option scheme
withPaymentIntent c = queryParam "payment_intent" (Just c)

withStartingAfter :: Text -> Option scheme
withStartingAfter c = queryParam "starting_after" (Just c)

withTransferGroup :: Text -> Option scheme
withTransferGroup c = queryParam "transfer_group" (Just c)

listCharges :: (HasReader "apiToken" ByteString m, MonadHttp m) => (Network.HTTP.Req.Option 'Https -> m ListChargesResponse)
listCharges = mkRequest listCharges'

listCharges' ::
  ( MonadHttp m,
    FromJSON a
  ) =>
  Network.HTTP.Req.Option 'Https ->
  m (JsonResponse a)
listCharges' =
  req
    GET
    (https "api.stripe.com" /: "v1" /: "charges")
    NoReqBody
    jsonResponse

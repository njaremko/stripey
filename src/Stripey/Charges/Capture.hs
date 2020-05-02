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

module Stripey.Charges.Capture
  ( captureCharge,
    withAmount,
    withReceiptEmail,
    withStatementDescriptor,
    withStatementDescriptorSuffix,
    defaultOptions,
  )
where

import Control.Carrier.Reader
import Data.Aeson hiding (defaultOptions)
import Network.HTTP.Req
import Protolude hiding (MonadReader, Option, Reader, ask)
import Stripey.Charges.Data.Charge (Charge)
import Stripey.Env

captureCharge :: (IsStripeRequest sig m, MonadHttp m) => Text -> (Network.HTTP.Req.Option 'Https -> m Charge)
captureCharge chargeId = mkRequest $ captureCharge' chargeId

captureCharge' ::
  ( MonadHttp m,
    FromJSON a
  ) =>
  Text ->
  Network.HTTP.Req.Option 'Https ->
  m (JsonResponse a)
captureCharge' chargeId =
  req
    POST
    (https "api.stripe.com" /: "v1" /: "charges" /~ chargeId /: "capture")
    NoReqBody
    jsonResponse

withAmount :: Int -> Option scheme
withAmount c = queryParam "amount" (Just c)

withReceiptEmail :: Text -> Option scheme
withReceiptEmail c = queryParam "receipt_email" (Just c)

withStatementDescriptor :: Text -> Option scheme
withStatementDescriptor c = queryParam "statement_descriptor" (Just c)

withStatementDescriptorSuffix :: Text -> Option scheme
withStatementDescriptorSuffix c = queryParam "statement_descriptor_suffix" (Just c)

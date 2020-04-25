{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Stripey.Charges.Capture
  (
    captureCharge,
    withAmount,
    withReceiptEmail,
    withStatementDescriptor,
    withStatementDescriptorSuffix,
    defaultOptions
    )
where

import Capability.Reader
import Data.Aeson hiding (defaultOptions)
import Network.HTTP.Req
import Stripey.Charges.Data.Charge (Charge)
import Stripey.Env
import Protolude hiding (MonadReader, Option, ask)

captureCharge :: (HasReader "apiToken" ByteString m, MonadHttp m) => Text -> (Network.HTTP.Req.Option 'Https -> m Charge)
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

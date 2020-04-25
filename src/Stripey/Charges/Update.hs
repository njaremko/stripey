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

module Stripey.Charges.Update
  (
    updateCharge,
    withCustomer,
    withDescription,
    withReceiptEmail,
    withShipping,
    withFraudDetails,
    defaultOptions
    )
where

import Capability.Reader
import Data.Aeson hiding (defaultOptions)
import Network.HTTP.Req
import Stripey.Charges.Data.Charge (Charge)
import Stripey.Env
import Protolude hiding (MonadReader, Option, ask)
import Stripey.Charges.Data.Shipping
import Stripey.Charges.Data.FraudDetails

updateCharge :: (HasReader "apiToken" ByteString m, MonadHttp m) => Text -> (Network.HTTP.Req.Option 'Https -> m Charge)
updateCharge chargeId = mkRequest $ updateCharge' chargeId

updateCharge' ::
  ( MonadHttp m,
    FromJSON a
  ) =>
  Text ->
  Network.HTTP.Req.Option 'Https ->
  m (JsonResponse a)
updateCharge' chargeId =
  req
    POST
    (https "api.stripe.com" /: "v1" /: "charges" /~ chargeId)
    NoReqBody
    jsonResponse

withCustomer :: Text -> Option scheme
withCustomer c = queryParam "customer" (Just c)

withDescription :: Text -> Option scheme
withDescription c = queryParam "description" (Just c)

withReceiptEmail :: Text -> Option scheme
withReceiptEmail c = queryParam "receipt_email" (Just c)

withShipping :: Shipping -> Option scheme
withShipping c = queryParam "shipping" (Just c)

withFraudDetails :: FraudDetails -> Option scheme
withFraudDetails c = queryParam "fraud_details" (Just c)


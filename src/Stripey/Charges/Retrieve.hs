{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Stripey.Charges.Retrieve
  (
    retrieveCharge,
    defaultOptions
    )
where

import Capability.Reader
import Data.Aeson hiding (defaultOptions)
import Network.HTTP.Req
import Stripey.Charges.Data.Charge (Charge)
import Stripey.Env
import Protolude hiding (MonadReader, Option, ask)

retrieveCharge :: (HasReader "apiToken" ByteString m, MonadHttp m) => Text -> (Network.HTTP.Req.Option 'Https -> m Charge)
retrieveCharge chargeId = mkRequest $ retrieveCharge' chargeId

retrieveCharge' ::
  ( MonadHttp m,
    FromJSON a
  ) =>
  Text ->
  Network.HTTP.Req.Option 'Https ->
  m (JsonResponse a)
retrieveCharge' chargeId =
  req
    GET
    (https "api.stripe.com" /: "v1" /: "charges" /~ chargeId)
    NoReqBody
    jsonResponse
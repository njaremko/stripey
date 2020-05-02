{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Customers.Create
  ( createCustomer,
    withDescription,
    withEmail,
    withName,
    withPaymentMethod,
    withPhone,
    withBalance,
    withCoupon,
    withSource,
    withTaxExempt,
  )
where

import Capability.Reader
import qualified Data.Aeson as Aeson
import Data.Aeson hiding (defaultOptions)
import qualified Data.Text as T
import Network.HTTP.Req
import Protolude hiding (Option)
import Stripey.Charges.Data.Charge (Charge)
import Stripey.Customers.Data.TaxExempt
import Stripey.Env

createCustomer :: (HasReader "apiToken" ByteString m, MonadHttp m) => (Network.HTTP.Req.Option 'Https -> m Charge)
createCustomer = mkRequest createCustomer'

createCustomer' ::
  MonadHttp m0 =>
  FromJSON a0 =>
  Network.HTTP.Req.Option 'Https ->
  m0 (JsonResponse a0)
createCustomer' = req POST (https "api.stripe.com" /: "v1" /: "customers") NoReqBody jsonResponse

withDescription :: Text -> Option scheme
withDescription c = queryParam "description" (Just c)

withEmail :: Text -> Option scheme
withEmail c = queryParam "email" (Just c)

withName :: Text -> Option scheme
withName c = queryParam "name" (Just c)

withPaymentMethod :: Text -> Option scheme
withPaymentMethod c = queryParam "payment_method" (Just c)

withPhone :: Text -> Option scheme
withPhone c = queryParam "phone" (Just c)

withBalance :: Int -> Option scheme
withBalance c = queryParam "balence" (Just c)

withCoupon :: Text -> Option scheme
withCoupon c = queryParam "coupon" (Just c)

withSource :: Text -> Option scheme
withSource c = queryParam "source" (Just c)

withTaxExempt :: TaxExempt -> Option scheme
withTaxExempt c = queryParam "tax_exempt" (Just c)

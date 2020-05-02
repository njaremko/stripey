{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Sessions.Create
  ( createSession,
    defaultOptions,
    withMode,
    withLineItems,
    withClientReferenceId,
    withCustomer,
    withCustomerEmail,
    withLocale,
    withSubmitType,
    withSubscriptionData,
  )
where

import Capability.Reader
import Data.Aeson (FromJSON)
import Network.HTTP.Req
import qualified Network.HTTP.Req as Req
import Protolude
import Stripey.Env
import qualified Stripey.Sessions.Data.LineItem as LI
import Stripey.Sessions.Data.LineItem (LineItem)
import Stripey.Sessions.Data.Locale (Locale)
import Stripey.Sessions.Data.Mode (Mode)
import Stripey.Sessions.Data.PaymentMethodType (PaymentMethodType)
import qualified Stripey.Sessions.Data.PaymentMethodType as PMT
import Stripey.Sessions.Data.Session (Session)
import Stripey.Sessions.Data.SubmitType (SubmitType)
import qualified Stripey.Sessions.Data.SubscriptionData as SD

createSession :: (HasReader "apiToken" ByteString m, MonadHttp m) => [PaymentMethodType] -> Text -> Text -> (Network.HTTP.Req.Option 'Https -> m Session)
createSession paymentMethodType successUrl cancelUrl = mkRequest (createSession' paymentMethodType successUrl cancelUrl)

createSession' ::
  MonadHttp m0 =>
  FromJSON a0 =>
  [PaymentMethodType] ->
  Text ->
  Text ->
  Req.Option 'Https ->
  m0 (JsonResponse a0)
createSession' paymentMethodTypes successUrl cancelUrl =
  req
    POST
    (https "api.stripe.com" /: "v1" /: "checkout" /: "sessions")
    (ReqBodyUrlEnc $ "success_url" =: successUrl <> "cancel_url" =: cancelUrl <> PMT.toStripeParams paymentMethodTypes)
    jsonResponse

withMode :: Mode -> Req.Option scheme
withMode c = queryParam "mode" (Just c)

withLineItems :: [LineItem] -> Req.Option scheme
withLineItems = LI.toStripeParams

withClientReferenceId :: Text -> Req.Option scheme
withClientReferenceId c = queryParam "client_reference_id" (Just c)

withCustomer :: Text -> Req.Option scheme
withCustomer c = queryParam "customer" (Just c)

withCustomerEmail :: Text -> Req.Option scheme
withCustomerEmail c = queryParam "customer_email" (Just c)

withLocale :: Locale -> Req.Option scheme
withLocale c = queryParam "locale" (Just c)

withSubmitType :: SubmitType -> Req.Option scheme
withSubmitType c = queryParam "submit_type" (Just c)

withSubscriptionData :: SD.SubscriptionData -> Req.Option scheme
withSubscriptionData = SD.toStripeParams

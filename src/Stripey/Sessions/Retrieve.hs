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

module Stripey.Sessions.Retrieve
  (
    retrieveSession
    )
where

import Capability.Reader
import Network.HTTP.Req
import qualified Network.HTTP.Req as Req
import Stripey.Sessions.Data.Session (Session)
import Stripey.Env
import Data.Aeson (FromJSON)
import Protolude hiding (MonadReader, ask)

retrieveSession :: (HasReader "apiToken" ByteString m, MonadHttp m) => Text -> (Req.Option 'Https -> m Session)
retrieveSession sessionId = mkRequest $ retrieveSession' sessionId

retrieveSession' ::
  ( MonadHttp m,
    FromJSON a
  ) =>
  Text ->
  Req.Option 'Https ->
  m (JsonResponse a)
retrieveSession' sessionId =
  req
    GET
    (https "api.stripe.com" /: "v1" /: "checkout" /: "sessions" /~ sessionId)
    NoReqBody
    jsonResponse

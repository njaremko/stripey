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

module Stripey.Sessions.Retrieve
  ( retrieveSession,
  )
where

import Capability.Reader
import Data.Aeson (FromJSON)
import Network.HTTP.Req
import qualified Network.HTTP.Req as Req
import Protolude hiding (MonadReader, ask)
import Stripey.Env
import Stripey.Sessions.Data.Session (Session)

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

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

module Stripey.Sessions.List
  ( listSessions,
    withLimit,
    withStartingAfter,
    withEndingBefore,
  )
where

import Capability.Reader
import Data.Aeson
import Network.HTTP.Req
import qualified Network.HTTP.Req as Req
import Protolude hiding (MonadReader, ask)
import Stripey.Env hiding (defaultOptions)
import Stripey.Sessions.Data.Session (Session)

data ListSessionsResponse = ListSessionsResponse
  { response_object :: Text,
    response_url :: Text,
    response_has_more :: Bool,
    response_data :: [Session]
  }
  deriving (Show, Generic)

instance FromJSON ListSessionsResponse where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 9}

listSessions :: (HasReader "apiToken" ByteString m, MonadHttp m) => (Req.Option 'Https -> m ListSessionsResponse)
listSessions = mkRequest listSessions'

listSessions' ::
  ( MonadHttp m,
    FromJSON a
  ) =>
  Req.Option 'Https ->
  m (JsonResponse a)
listSessions' =
  req
    GET
    (https "api.stripe.com" /: "v1" /: "checkout" /: "sessions")
    NoReqBody
    jsonResponse

withLimit :: Int -> Req.Option scheme
withLimit c = queryParam "limit" (Just c)

withStartingAfter :: Text -> Req.Option scheme
withStartingAfter c = queryParam "starting_after" (Just c)

withEndingBefore :: Text -> Req.Option scheme
withEndingBefore c = queryParam "ending_before" (Just c)

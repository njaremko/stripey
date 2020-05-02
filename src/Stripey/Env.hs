{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Stripey.Env
  ( Env (..),
    mkEnv,
    mkRequest,
    runStripe,
    defaultOptions,
    StripeRequest,
  )
where

import Capability.Reader
import Capability.Source
import qualified Data.Aeson as Aeson
import Network.HTTP.Req
import Protolude hiding (MonadReader, Option, ask)

data Env = Env
  { apiToken :: ByteString,
    httpConfig :: HttpConfig
  }
  deriving (Generic)

defaultOptions :: Monoid a => a
defaultOptions = mempty

newtype StripeRequest a = StripeRequest (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO) via ReaderT Env IO
  deriving
    (HasSource "apiToken" ByteString, HasReader "apiToken" ByteString)
    via Field "apiToken" "env" (MonadReader (ReaderT Env IO))
  deriving
    (HasSource "httpConfig" HttpConfig, HasReader "httpConfig" HttpConfig)
    via Field "httpConfig" "env" (MonadReader (ReaderT Env IO))

instance MonadHttp StripeRequest where
  handleHttpException = StripeRequest . lift . throwIO
  getHttpConfig = ask @"httpConfig"

runStripe :: Env -> StripeRequest a -> IO a
runStripe env (StripeRequest m) = runReaderT m env

mkEnv :: ByteString -> Env
mkEnv token = Env {apiToken = "Bearer " <> token, httpConfig = defaultHttpConfig}

mkRequest ::
  ( HasReader "apiToken" ByteString m,
    MonadHttp m,
    Aeson.FromJSON a
  ) =>
  (Option 'Https -> m (JsonResponse a)) ->
  Option 'Https ->
  m a
mkRequest re options = do
  token <- ask @"apiToken"
  r <- (re $ header "Authorization" token <> options)
  liftIO $ return (responseBody r)

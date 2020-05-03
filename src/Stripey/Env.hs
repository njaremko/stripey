{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Stripey.Env
  ( StripeEnv (..),
    HasApiToken (..),
    StripeRequest,
    IsStripeRequest,
    mkEnv,
    mkRequest,
    runStripe,
    defaultOptions,
  )
where

import Control.Carrier.Lift
import Control.Carrier.Reader
import qualified Data.Aeson as Aeson
import Network.HTTP.Req
import Protolude hiding (Option, Reader, ask, runReader)

data StripeEnv = StripeEnv
  { apiToken :: ByteString,
    httpConfig :: HttpConfig
  }
  deriving (Generic)

class HasApiToken a where
  getApiToken :: a -> ByteString

instance HasApiToken ByteString where
  getApiToken = identity

instance HasApiToken StripeEnv where
  getApiToken = apiToken

defaultOptions :: Monoid a => a
defaultOptions = mempty

type IsStripeRequest sig m = (Has (Reader StripeEnv) sig m)

type StripeRequest = (ReaderC StripeEnv (LiftC IO))

instance MonadHttp StripeRequest where
  handleHttpException = lift . throwIO
  getHttpConfig = do
    StripeEnv {httpConfig} <- ask
    return httpConfig

runStripe :: StripeEnv -> StripeRequest a -> IO a
runStripe env = runM . runReader @StripeEnv env

mkEnv :: ByteString -> StripeEnv
mkEnv token = StripeEnv {apiToken = "Bearer " <> token, httpConfig = defaultHttpConfig}

mkRequest ::
  ( IsStripeRequest sig m,
    HasApiToken StripeEnv,
    MonadHttp m,
    Aeson.FromJSON a
  ) =>
  (Option 'Https -> m (JsonResponse a)) ->
  Option 'Https ->
  m a
mkRequest re options = do
  env <- ask @StripeEnv
  r <- re $ header "Authorization" (getApiToken env) <> options
  liftIO $ return (responseBody r)

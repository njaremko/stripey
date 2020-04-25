{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Stripey.Sessions.Data.SubmitType
  ( SubmitType,
  )
where

import Data.Aeson
import Data.Char (toLower)
import Protolude
import Web.Internal.HttpApiData (ToHttpApiData(..))
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

data SubmitType = Auto | Pay | Book | Donate deriving (Show, Generic)

instance FromJSON SubmitType where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

instance ToJSON SubmitType where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

instance ToHttpApiData SubmitType where
  toUrlPiece shipping = T.dropAround (=='"') . TL.toStrict $ TLE.decodeUtf8 (encode shipping)
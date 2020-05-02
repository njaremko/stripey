{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Stripey.Charges.Data.Currency
  ( Currency (..),
  )
where

import Data.Aeson
import Data.Char (toLower)
import Protolude

data Currency
  = USD
  | AED
  | AFN
  | ALL
  | AMD
  | ANG
  | AOA
  | ARS
  | AUD
  | AWG
  | AZN
  | BAM
  | BBD
  | BDT
  | BGN
  | BIF
  | BMD
  | BND
  | BOB
  | BRL
  | BSD
  | BWP
  | BZD
  | CAD
  | CDF
  | CHF
  | CLP
  | CNY
  | COP
  | CRC
  | CVE
  | CZK
  | DJF
  | DKK
  | DOP
  | DZD
  | EGP
  | ETB
  | EUR
  | FJD
  | FKP
  | GBP
  | GEL
  | GIP
  | GMD
  | GNF
  | GTQ
  | GYD
  | HKD
  | HNL
  | HRK
  | HTG
  | HUF
  | IDR
  | ILS
  | INR
  | ISK
  | JMD
  | JPY
  | KES
  | KGS
  | KHR
  | KMF
  | KRW
  | KYD
  | KZT
  | LAK
  | LBP
  | LKR
  | LRD
  | LSL
  | MAD
  | MDL
  | MGA
  | MKD
  | MMK
  | MNT
  | MOP
  | MRO
  | MUR
  | MVR
  | MWK
  | MXN
  | MYR
  | MZN
  | NAD
  | NGN
  | NIO
  | NOK
  | NPR
  | NZD
  | PAB
  | PEN
  | PGK
  | PHP
  | PKR
  | PLN
  | PYG
  | QAR
  | RON
  | RSD
  | RUB
  | RWF
  | SAR
  | SBD
  | SCR
  | SEK
  | SGD
  | SHP
  | SLL
  | SOS
  | SRD
  | STD
  | SZL
  | THB
  | TJS
  | TOP
  | TRY
  | TTD
  | TWD
  | TZS
  | UAH
  | UGX
  | UYU
  | UZS
  | VND
  | VUV
  | WST
  | XAF
  | XCD
  | XOF
  | XPF
  | YER
  | ZAR
  | ZMW
  deriving (Show, Generic)

instance FromJSON Currency where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

instance ToJSON Currency where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where


import Stripey.Charges.Data.CardBrand
import Data.Aeson
import Test.Hspec
import Test.Validity.Aeson
import Protolude

spec :: Spec
spec =
  describe "CardBrand" $
    jsonSpecOnValid @CardBrand

main :: IO ()
main = hspec $ do
  spec

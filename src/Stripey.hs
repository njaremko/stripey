{-|
Module      :  Stripey
Copyright   :  © 2020–present Nathan Jaremko
License     :  BSD 3 clause

Maintainer  :  Nathan Jaremko <nathan@jaremko.ca>
Stability   :  experimental
Portability :  portable

=== About the library

Stripey is a modern, easy-to-use, type-safe, extendable, high-level HTTP
client targeting the Stripe API as of March 2020.

Some general guidelines:

- Import 'Stripey.Env' unqualified

- Each Stripe API is inside it's own module.

- It's recommended to import each API module as qualified.

- Functions that call Stripe APIs have all their mandatory arguments provided in the type signature.

- Optional arguments are added using the withValue functions and joined with '<>'.

@
&#x7b;-\# LANGUAGE DeriveGeneric \#-&#x7d;
&#x7b;-\# LANGUAGE DuplicateRecordFields \#-&#x7d;
&#x7b;-\# LANGUAGE FlexibleContexts \#-&#x7d;
&#x7b;-\# LANGUAGE FlexibleInstances \#-&#x7d;
&#x7b;-\# LANGUAGE OverloadedStrings \#-&#x7d;
&#x7b;-\# LANGUAGE OverloadedLabels \#-&#x7d;

module Main (main) where

import qualified Stripey.Charges.List as CL
import qualified Stripey.Charges.Create as CC
import qualified Stripey.Charges.Retrieve as CR
import qualified Stripey.Sessions.Create as SC
import Stripey.Charges.Data.Currency ( Currency(CAD) )
import Stripey.Sessions.Data.PaymentMethodType (PaymentMethodType (Card))
import Stripey.Sessions.Data.LineItem
import Stripey.Sessions.Data.Mode ( Mode(Payment))
import Stripey.Env
import Protolude
import Stripey.Charges.Data.Charge (Charge)

makeCharge :: Text -> StripeRequest Charge
makeCharge customer = CC.createCharge 10 CAD
   $ CC.withDescription "this is a test!"
   <> CC.withCustomer customer
main :: IO ()
main = do
  let env = mkEnv "sk_test_...."
  runStripe env $ do
    createdSession <- SC.createSession [Card] "https://example.com/success" "https://example.com/cancel"
       $ SC.withMode Payment
       <>   SC.withLineItems [LineItem 100 CAD "Example 1" 1 Nothing Nothing]
    print createdSession
    singletonListForCustomer <- CL.listCharges $ CL.withLimit 1 <>   CL.withCustomer "cus_GCL2qiq19I4Cqu"
    print singletonListForCustomer
    listOfCharges <- CL.listCharges defaultOptions
    print listOfCharges
    retrievedCharge <- CR.retrieveCharge "ch_1GTG7gKz9NjFstHCziw56Hxf" defaultOptions
    print retrievedCharge
    createdCharge <- makeCharge "cus_GCL2qiq19I4Cqu"
    print createdCharge
@
-}
module Stripey () where

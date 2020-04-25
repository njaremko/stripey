# stripe

[![GitHub CI](https://github.com/njaremko/stripe/workflows/CI/badge.svg)](https://github.com/njaremko/stripey/actions)
[![Build status](https://img.shields.io/travis/njaremko/stripe.svg?logo=travis)](https://travis-ci.org/njaremko/stripey)
[![Hackage](https://img.shields.io/hackage/v/stripe.svg?logo=haskell)](https://hackage.haskell.org/package/stripey)
[![Stackage Lts](http://stackage.org/package/stripe/badge/lts)](http://stackage.org/lts/package/stripey)
[![Stackage Nightly](http://stackage.org/package/stripe/badge/nightly)](http://stackage.org/nightly/package/stripey)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

A modern stripe library for Haskell

The goal of library is to implement the entire modern (`2020-03-02`) Stripe API.

Implemented So Far:
- Core Resources
    - [ ] Balance
    - [ ] Balance Transactions
    - [x] Charges
    - [ ] Customers
    - [ ] Disputes
    - [ ] Events
    - [ ] Files
    - [ ] File Links
    - [ ] Mandates
    - [ ] PaymentIntents
    - [ ] SetupIntents
    - [ ] Payouts
    - [ ] Products
    - [ ] Refunds
    - [ ] Tokens
- Payment Methods
    - [ ] PaymentMethods
    - [ ] Bank Accounts
    - [ ] Cards
    - [ ] Sources
- Checkout
    - [ ] Sessions
- Billing
    - [ ] Coupons
    - [ ] Credit Notes
    - [ ] Customer Balance Transactions
    - [ ] Customer Tax IDs
    - [ ] Discounts
    - [ ] Invoices
    - [ ] Invoice Items
    - [ ] Plans
    - [ ] Products
    - [ ] Subscriptions
    - [ ] Subscription Items
    - [ ] Subscription Schedules
    - [ ] Tax Rates
    - [ ] Usage Records
- Connect
    - [ ] Accounts
    - [ ] Account Links
    - [ ] Application Fees
    - [ ] Application Fee Refunds
    - [ ] Capabilities
    - [ ] Country Specs
    - [ ] External Accounts
    - [ ] Persons
    - [ ] Top-Ups
    - [ ] Transfers
    - [ ] Transfer Reversals
- Fraud
    - [ ] Early Fraud Warnings
    - [ ] Reviews
    - [ ] Value Lists
    - [ ] Value List Items
- Issuing
    - [ ] Authorizations
    - [ ] Cardholders
    - [ ] Cards
    - [ ] Transactions
- Terminal
    - [ ] Connection Tokens
    - [ ] Locations
    - [ ] Readers
- Orders
    - [ ] Orders
    - [ ] Order Items
    - [ ] Returns
    - [ ] SKUs
- Sigma
    - [ ] Scheduled Queries
- Reporting
    - [ ] Report Runs
    - [ ] Report Types
- Webhooks
    - [ ] Webhook Endpoints

# FAQs
## Why does this exist when there's already a haskell stripe library?
The [dmjio/stripe](https://github.com/dmjio/stripe) library is pinned to Stripe's `2014-10-07` API version, so it's missing a lot of features in the modern stripe API like:
- Connect Support
- Sources Support
- Checkout Support
- Sigma Support
- Everything else that Stripe has done since `2014-10-07`

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module OtherExamples.InnerJoin where

import Types.Payment
import Types.Customer

import Opaleye as O

example1 = O.orderBy (O.asc (\(_, _, _, _, d) -> d)) $ do
  customer <- O.selectTable customerTable
  payment  <- O.selectTable paymentTable

  O.where_ (cCustomerId customer .=== pCustomerId payment)

  pure (cCustomerId customer, cFirstName customer, cLastName customer,
        pAmount payment, pPaymentDate payment)

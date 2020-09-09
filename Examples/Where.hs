{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}

-- See https://www.postgresqltutorial.com/postgresql-where/

module Examples.Where where

import qualified Opaleye as O

import           Types.Customer

import           Opaleye ((.==), (./=), (.<=), (.&&), (.||))
example1 = do
  customer <- O.selectTable customerTable
  O.where_ (cFirstName customer .== O.sqlString "Jamie")
  pure (cFirstName customer, cLastName customer)

example2 = do
  customer <- O.selectTable customerTable
  O.where_ (cFirstName customer .== O.sqlString "Jamie"
         .&& cLastName customer .== O.sqlString "Waugh")
  pure (cFirstName customer, cLastName customer)

example3 = do
  customer <- O.selectTable customerTable
  O.where_ (cLastName customer .== O.sqlString "Rodriguez"
         .|| cFirstName customer .== O.sqlString "Adam")
  pure (cFirstName customer, cLastName customer)

example4 = do
  customer <- O.selectTable customerTable
  O.where_ (O.in_ (map O.sqlString ["Ann", "Anna", "Annie"]) (cFirstName customer))
  pure (cFirstName customer, cLastName customer)

example5 = do
  customer <- O.selectTable customerTable
  O.where_ (cFirstName customer `O.like` O.sqlString "Ann%")
  pure (cFirstName customer, cLastName customer)

example6 = do
  customer <- O.selectTable customerTable
  let l = O.sqlLength (cFirstName customer)
  O.where_ (cFirstName customer `O.like` O.sqlString "A%"
          .&& 3 .<= l .&& l .<= 5)
  pure (cFirstName customer, O.sqlLength (cFirstName customer))

example7 = do
  customer <- O.selectTable customerTable
  O.where_ (cFirstName customer `O.like` O.sqlString "Bra%"
          .&& cLastName customer ./= O.sqlString "Motley")
  pure (cFirstName customer, cLastName customer)

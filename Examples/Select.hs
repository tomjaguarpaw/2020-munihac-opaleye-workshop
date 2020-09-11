{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- https://www.postgresqltutorial.com/postgresql-select/

module Examples.Select where

import qualified Opaleye as O
import           Opaleye ((.++))

import           Types.Customer

example1 = do
  customer <- O.selectTable customerTable
  pure (cFirstName customer)

example2 = do
  customer <- O.selectTable customerTable
  pure (cFirstName customer, cLastName customer, cEmail customer)

example3 = do
  customer <- O.selectTable customerTable
  pure customer

example4 = do
  customer <- O.selectTable customerTable
  pure (cFirstName customer .++ O.sqlString " " .++ cLastName customer,
        cEmail customer)

example5 :: O.Select (O.Field O.SqlInt4)
example5 = do
  pure (5 * 3)

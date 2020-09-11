{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- See https://www.postgresqltutorial.com/postgresql-order-by/

module Examples.OrderBy where

import qualified Opaleye as O

import           Types.Customer

example1 = do
  customer <- O.orderBy (O.asc cFirstName) (O.selectTable customerTable)
  pure (cFirstName customer, cLastName customer)

example2 = do
  customer <- O.orderBy (O.desc cLastName) (O.selectTable customerTable)
  pure (cFirstName customer, cLastName customer)

example3 = do
  customer <- O.orderBy (O.asc cFirstName <> O.desc cLastName)
                        (O.selectTable customerTable)
  pure (cFirstName customer, cLastName customer)

example4 = do
  customer <- O.orderBy (O.desc (O.sqlLength . cFirstName))
                        (O.selectTable customerTable)
  pure (cFirstName customer, O.sqlLength (cFirstName customer))

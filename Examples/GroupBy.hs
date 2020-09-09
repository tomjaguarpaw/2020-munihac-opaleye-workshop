{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}

-- See https://www.postgresqltutorial.com/postgresql-group-by/

module Examples.GroupBy where

import qualified Opaleye as O
import           Opaleye ((.==), (.++))

import           Types.Customer
import           Types.Payment

import qualified Data.Profunctor as P

example1 =
  O.aggregate (P.lmap pCustomerId O.groupBy) (O.selectTable paymentTable)

example2 =
  O.aggregate ((,) <$> P.lmap pCustomerId O.groupBy
                   <*> P.lmap pAmount O.sum)
              (O.selectTable paymentTable)

example3 = O.aggregate ((,) <$> P.lmap fst O.groupBy
                                    <*> P.lmap snd O.sum) $ do
  payment  <- O.selectTable paymentTable
  customer <- O.selectTable customerTable
  O.where_ (pCustomerId payment .== cCustomerId customer)
  pure (cFirstName customer .++ O.sqlString " " .++ cLastName customer,
        pAmount payment)

example4 = O.aggregate ((,) <$> P.lmap pStaffId O.groupBy
                                    <*> P.lmap pPaymentDate O.count) $ do
  payment <- O.selectTable paymentTable
  pure payment

example5 =
  O.orderBy (O.asc (\(customerId, _, _) -> customerId)) $
  O.aggregate ((,,) <$> P.lmap pCustomerId O.groupBy
                                     <*> P.lmap pStaffId O.groupBy
                                     <*> P.lmap pAmount O.sum) $ do
  payment <- O.selectTable paymentTable
  pure payment

example6 = O.aggregate ((,) <$> P.lmap fst O.groupBy
                                    <*> P.lmap snd O.sum) $ do
  payment <- O.selectTable paymentTable
  pure (O.dateOfTimestamp (pPaymentDate payment), pAmount payment)

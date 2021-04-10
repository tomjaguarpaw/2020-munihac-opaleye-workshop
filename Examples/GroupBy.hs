{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE Arrows #-}

-- See https://www.postgresqltutorial.com/postgresql-group-by/

module Examples.GroupBy where

import qualified Opaleye as O
import           Opaleye ((.===), (.++))

import           Types.Customer
import           Types.Payment

import           Data.Profunctor.Product.Newtype (pNewtype)
import qualified Data.Profunctor as P

import Control.Arrow

example1 =
  O.aggregate (P.lmap pCustomerId (pNewtype O.groupBy))
              (O.selectTable paymentTable)

example2 =
  O.aggregateEasy $ do
    payment <- O.selectTable paymentTable
    pure (O.agg (pNewtype O.groupBy) (pCustomerId payment),
          O.agg O.sum (pAmount payment))

-- breaks if we don't make aggregateEasy use lateral
example2_with_arrows =
  proc () -> do
    payment <- O.selectTable paymentTable -< ()
    whatever <- O.aggregateEasy (proc payment -> do
      returnA -< (O.agg (pNewtype O.groupBy) (pCustomerId payment),
                  O.agg O.sum (pAmount payment))) -< payment
    O.restrict -< snd whatever O..== O.unsafeCoerceField (O.toFields (0.99 :: Double) :: O.Field O.SqlFloat8)
    returnA -< whatever

example3 = O.aggregateEasy $ do
  payment  <- O.selectTable paymentTable
  customer <- O.selectTable customerTable
  O.where_ (pCustomerId payment .=== cCustomerId customer)
  pure (O.agg O.groupBy (cFirstName customer .++ O.sqlString " " .++ cLastName customer),
        O.agg O.sum (pAmount payment))

example4 = O.aggregateEasy $ do
  payment <- O.selectTable paymentTable
  pure (O.agg O.groupBy (pStaffId payment),
        O.agg O.count (pPaymentDate payment))

example5 =
  O.orderBy (O.asc (\(CustomerId customerId, _, _) -> customerId)) $
  O.aggregateEasy $ do
  payment <- O.selectTable paymentTable
  pure (O.agg (pNewtype O.groupBy) (pCustomerId payment),
        O.agg O.groupBy (pStaffId payment),
        O.agg O.sum (pAmount payment))

example6 = O.aggregateEasy $ do
  payment <- O.selectTable paymentTable
  pure (O.agg O.groupBy (O.dateOfTimestamp (pPaymentDate payment)),
        O.agg O.sum (pAmount payment))

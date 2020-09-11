{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Payment where

import qualified Opaleye as O

import           Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)

data Payment' a b c d e f = Payment
  { pPaymentId   :: a
  , pCustomerId  :: b
  , pStaffId     :: c
  , pRentalId    :: d
  , pAmount      :: e
  , pPaymentDate :: f
  }
  deriving Show

type PaymentW = Payment' (O.Field O.SqlInt4)
                         (O.Field O.SqlInt4)
                         (O.Field O.SqlInt4)
                         (O.Field O.SqlInt4)
                         (O.Field O.SqlNumeric)
                         (O.Field O.SqlTimestamp)

$(makeAdaptorAndInstanceInferrable "pPayment" ''Payment')

paymentTable :: O.Table PaymentW PaymentW
paymentTable = O.table "payment" (pPayment (Payment
    { pPaymentId   = O.tableField "payment_id"
    , pCustomerId  = O.tableField "customer_id"
    , pStaffId     = O.tableField "staff_id"
    , pRentalId    = O.tableField "rental_id"
    , pAmount      = O.tableField "amount"
    , pPaymentDate = O.tableField "payment_date"
    }))

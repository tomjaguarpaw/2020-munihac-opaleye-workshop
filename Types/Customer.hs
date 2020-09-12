{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Customer where

import qualified Opaleye as O

import           Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)

data Customer' a b c d e f g h i j = Customer
  { cCustomerId :: a
  , cStoreId    :: b
  , cFirstName  :: c
  , cLastName   :: d
  , cEmail      :: e
  , cAddressId  :: f
  , cActiveBool :: g
  , cCreateDate :: h
  , cLastUpdate :: i
  , cActive     :: j
  }
  deriving Show

data CustomerId a = CustomerId a deriving Show

type CustomerW = Customer' (CustomerId (Maybe (O.Field O.SqlInt4)))
                           (O.Field O.SqlInt4)
                           (O.Field O.SqlText)
                           (O.Field O.SqlText)
                           (O.FieldNullable O.SqlText)
                           (O.Field O.SqlInt4)
                           (O.Field O.SqlBool)
                           (O.Field O.SqlDate)
                           (Maybe (O.Field O.SqlTimestamp))
                           (O.FieldNullable O.SqlInt4)

type CustomerR = Customer' (CustomerId (O.Field O.SqlInt4))
                           (O.Field O.SqlInt4)
                           (O.Field O.SqlText)
                           (O.Field O.SqlText)
                           (O.FieldNullable O.SqlText)
                           (O.Field O.SqlInt4)
                           (O.Field O.SqlBool)
                           (O.Field O.SqlDate)
                           (O.Field O.SqlTimestamp)
                           (O.FieldNullable O.SqlInt4)

$(makeAdaptorAndInstanceInferrable "pCustomer" ''Customer')
$(makeAdaptorAndInstanceInferrable "pCustomerId_" ''CustomerId)

customerTable :: O.Table CustomerW CustomerR
customerTable = O.table "customer" (pCustomer (Customer
    { cCustomerId =
        pCustomerId_ (CustomerId (O.tableField "customer_id"))
    , cStoreId    = O.tableField "store_id"
    , cFirstName  = O.tableField "first_name"
    , cLastName   = O.tableField "last_name"
    , cEmail      = O.tableField "email"
    , cAddressId  = O.tableField "address_id"
    , cActiveBool = O.tableField "activebool"
    , cCreateDate = O.tableField "create_date"
    , cLastUpdate = O.tableField "last_update"
    , cActive     = O.tableField "active"
    }))

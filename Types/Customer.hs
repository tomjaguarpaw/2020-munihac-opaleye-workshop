{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}

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

type CustomerW = Customer' (O.Field O.SqlInt4)
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

customerTable :: O.Table CustomerW CustomerW
customerTable = O.table "customer" (pCustomer (Customer
    { cCustomerId = O.tableField "customer_id"
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

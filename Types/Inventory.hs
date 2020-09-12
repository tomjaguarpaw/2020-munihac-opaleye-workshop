{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Inventory where

import qualified Opaleye as O

import           Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)

data Inventory' a b c d = Inventory
  { pInventoryId :: a
  , pFilmId      :: b
  , pStoreId     :: c
  , pLastUpdate  :: d
  }
  deriving Show

type InventoryW = Inventory' (O.Field O.SqlInt4)
                             (O.Field O.SqlInt4)
                             (O.Field O.SqlInt4)
                             (O.Field O.SqlTimestamp)

type InventoryNullable =
  Inventory' (O.FieldNullable O.SqlInt4)
             (O.FieldNullable O.SqlInt4)
             (O.FieldNullable O.SqlInt4)
             (O.FieldNullable O.SqlTimestamp)

$(makeAdaptorAndInstanceInferrable "pInventory" ''Inventory')

inventoryTable :: O.Table InventoryW InventoryW
inventoryTable = O.table "inventory" (pInventory (Inventory
    { pInventoryId = O.tableField "inventory_id"
    , pFilmId      = O.tableField "film_id"
    , pStoreId     = O.tableField "store_id"
    , pLastUpdate  = O.tableField "last_update"
    }))

{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Options as Options
import qualified Database.Postgres.Temp as T
import qualified Opaleye as O
import           Opaleye ((.==), (./=), (.>), (.>=), (.<), (.<=), (.&&), (.||),
                          (.++))
import qualified Opaleye.Join as J
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D
import           Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable')

import           Control.Exception (bracket)
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.String (fromString)
import           GHC.Int (Int64)
import           System.Process (callProcess)

tarFile :: FilePath
tarFile = "/home/tom/Haskell/haskell-opaleye/sql/dvdrental/dvdrental.tar"

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

type CustomerW = Customer' (O.Field O.SqlInt8)
                           (O.Field O.SqlInt4)
                           (O.Field O.SqlText)
                           (O.Field O.SqlText)
                           (O.FieldNullable O.SqlText)
                           (O.Field O.SqlInt4)
                           (O.Field O.SqlBool)
                           (O.Field O.SqlDate)
                           (O.Field O.SqlTimestamp)
                           (O.FieldNullable O.SqlInt8)

$(makeAdaptorAndInstanceInferrable' ''Customer')

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

example5 = do
  pure (5 * 3 :: O.Field O.SqlInt4)

printNumberedRows :: Show a => [a] -> IO ()
printNumberedRows = mapM_ print . zip [1..]

main :: IO ()
main = withDvdRentalConnection $ \conn -> do
  printNumberedRows =<< O.runSelectI conn example1
  printNumberedRows =<< O.runSelectI conn example2
  printNumberedRows =<< O.runSelectI conn example3
  printNumberedRows =<< O.runSelectI conn example4
  printNumberedRows =<< O.runSelectI conn example5

withDvdRentalConnection :: Show r => (PGS.Connection -> IO r) -> IO ()
withDvdRentalConnection f = do
  -- It's important that the user be postgres because the restore
  -- script explicitly assigns roles to postgres.  (It also explicitly
  -- calls the database dvdrental, but it nonetheless seems happy to
  -- create it in the postgres db.  I don't know why.)
  let config = T.optionsToDefaultConfig mempty { Options.user = pure "postgres" }

  putStr "Starting temporary DB..."

  e <- T.withConfig config $ \db -> do
    let connstr = T.toConnectionString db
        connchars = unpack connstr

    putStr "restoring data..."
    callProcess "pg_restore" ["--dbname", connchars, tarFile]

    withConnectPostgreSQL connstr $ \conn -> do
      putStrLn "ready to interact with DB."

      f conn

  case e of
    Left l -> putStr "Error: " >> print l
    Right r -> putStr "Result: " >> print r

where_ = O.viaLateral O.restrict

withConnectPostgreSQL :: ByteString -> (PGS.Connection -> IO c) -> IO c
withConnectPostgreSQL connstr =
  bracket (PGS.connectPostgreSQL connstr) PGS.close

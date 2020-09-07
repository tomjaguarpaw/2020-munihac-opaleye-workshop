{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import qualified Opaleye as O
import           Opaleye ((.==), (./=), (.>), (.>=), (.<), (.<=), (.&&), (.||),
                          (.++))
import           Opaleye.Experimental.Enum (fromFieldToFieldsEnum)
--import qualified Opaleye.Join as J

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D
import           Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)

import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Options as Options
import qualified Database.Postgres.Temp as T

import           Control.Exception (bracket)
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
--import           Data.String (fromString)
--import           GHC.Int (Int64)
import           System.Process (callProcess)

-- Remove
import           Opaleye.Internal.Inferrable

data SqlRating

data Rating = G | PG | PG13 | R | NC17 deriving Show

toSqlRatingString :: Rating -> String
toSqlRatingString r = case r of
    G    -> "G"
    PG   -> "PG"
    PG13 -> "PG-13"
    R    -> "R"
    NC17 -> "NC-17"

fromSqlRatingString :: String -> Maybe Rating
fromSqlRatingString s = case s of
    "G"     -> Just G
    "PG"    -> Just PG
    "PG-13" -> Just PG13
    "R"     -> Just R
    "NC-17" -> Just NC17
    _       -> Nothing

(fromFieldRating, toFieldsRating) =
  fromFieldToFieldsEnum "mpaa_rating" fromSqlRatingString toSqlRatingString

instance O.DefaultFromField SqlRating Rating where
  defaultFromField = fromFieldRating

instance rating ~ Rating
  => D.Default (Inferrable O.FromFields) (O.Column SqlRating) rating where
  def = Inferrable D.def

instance D.Default O.ToFields Rating (O.Column SqlRating) where
  def = toFieldsRating

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

data Film' a b c d e f g h i j k l = Film
  { fFilmId          :: a
  , fTitle           :: b
  , fDescription     :: c
  , fReleaseYear     :: d
  , fLanguageId      :: e
  , fRentalDuration  :: f
  , fRentalRate      :: g
  , fLength          :: h
  , fReplacementCost :: i
  , fRating          :: j
  , fLastUpdate      :: k
  , fSpecialFeatures :: l
  }
  deriving Show

type FilmW = Film' (O.Field O.SqlInt4)
                   (O.Field O.SqlText)
                   (O.Field O.SqlText)
                   (O.Field O.SqlInt4)
                   (O.Field O.SqlInt4)
                   (O.Field O.SqlNumeric)
                   (O.Field O.SqlNumeric)
                   (O.Field O.SqlInt4)
                   (O.Field O.SqlNumeric)
                   (O.Field SqlRating)
                   (O.Field O.SqlTimestamp)
                   (O.Field (O.SqlArray O.SqlText))

$(makeAdaptorAndInstanceInferrable "pFilm" ''Film')

filmTable :: O.Table FilmW FilmW
filmTable = O.table "film" (pFilm (Film
    { fFilmId          = O.tableField "film_id"
    , fTitle           = O.tableField "title"
    , fDescription     = O.tableField "description"
    , fReleaseYear     = O.tableField "release_year"
    , fLanguageId      = O.tableField "language_id"
    , fRentalDuration  = O.tableField "rental_duration"
    , fRentalRate      = O.tableField "rental_rate"
    , fLength          = O.tableField "length"
    , fReplacementCost = O.tableField "replacement_cost"
    , fRating          = O.tableField "rating"
    , fLastUpdate      = O.tableField "last_update"
    , fSpecialFeatures = O.tableField "special_features"
    }))

insertFilm conn = O.runInsert_ conn O.Insert
  { O.iTable      = filmTable
  , O.iRows       = [ Film { fFilmId          = 9999
                           , fTitle           = O.sqlString "My title"
                           , fDescription     = O.sqlString "My description"
                           , fReleaseYear     = 2001
                           , fLanguageId      = 1
                           , fRentalDuration  = 66
                           , fRentalRate      = 55
                           , fLength          = 44
                           , fReplacementCost = 33
                           , fRating          = O.toFields PG
                           , fLastUpdate      = timestampOfString "2020-01-01"
                           , fSpecialFeatures = O.sqlArray id []
                           }]
  , O.iReturning  = O.rCount
  , O.iOnConflict = Nothing
  }

salesByFilmCategoryView :: O.Select (O.Field O.SqlText, O.Field O.SqlNumeric)
salesByFilmCategoryView =
  O.selectTable (O.table "sales_by_film_category"
                   (PP.p2 (O.requiredTableField "category",
                           O.requiredTableField "total_sales")))

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

exampleOrderBy_1 = do
  customer <- O.orderBy (O.asc cFirstName) (O.selectTable customerTable)
  pure (cFirstName customer, cLastName customer)

exampleOrderBy_2 = do
  customer <- O.orderBy (O.desc cLastName) (O.selectTable customerTable)
  pure (cFirstName customer, cLastName customer)

exampleOrderBy_3 = do
  customer <- O.orderBy (O.asc cFirstName <> O.desc cLastName)
                        (O.selectTable customerTable)
  pure (cFirstName customer, cLastName customer)

exampleOrderBy_4 = do
  customer <- O.orderBy (O.desc (O.sqlLength . cFirstName))
                        (O.selectTable customerTable)
  pure (cFirstName customer, O.sqlLength (cFirstName customer))

example3_1 = do
  customer <- O.selectTable customerTable
  where_ (cFirstName customer .== O.sqlString "Jamie")
  pure (cFirstName customer, cLastName customer)

example3_2 = do
  customer <- O.selectTable customerTable
  where_ (cFirstName customer .== O.sqlString "Jamie"
         .&& cLastName customer .== O.sqlString "Waugh")
  pure (cFirstName customer, cLastName customer)

example3_3 = do
  customer <- O.selectTable customerTable
  where_ (cLastName customer .== O.sqlString "Rodriguez"
         .|| cFirstName customer .== O.sqlString "Adam")
  pure (cFirstName customer, cLastName customer)

example3_4 = do
  customer <- O.selectTable customerTable
  where_ (O.in_ (map O.sqlString ["Ann", "Anna", "Annie"]) (cFirstName customer))
  pure (cFirstName customer, cLastName customer)

example3_5 = do
  customer <- O.selectTable customerTable
  where_ (cFirstName customer `O.like` O.sqlString "Ann%")
  pure (cFirstName customer, cLastName customer)

example3_6 = do
  customer <- O.selectTable customerTable
  let l = O.sqlLength (cFirstName customer)
  where_ (cFirstName customer `O.like` O.sqlString "A%"
          .&& 3 .<= l .&& l .<= 5)
  pure (cFirstName customer, O.sqlLength (cFirstName customer))

example3_7 = do
  customer <- O.selectTable customerTable
  where_ (cFirstName customer `O.like` O.sqlString "Bra%"
          .&& cLastName customer ./= O.sqlString "Motley")
  pure (cFirstName customer, cLastName customer)

exampleGroupBy_1 =
  O.aggregate (P.lmap pCustomerId O.groupBy) (O.selectTable paymentTable)

exampleGroupBy_2 =
  O.aggregate ((,) <$> P.lmap pCustomerId O.groupBy
                   <*> P.lmap pAmount O.sum)
              (O.selectTable paymentTable)

exampleGroupBy_3 = O.aggregate ((,) <$> P.lmap fst O.groupBy
                                    <*> P.lmap snd O.sum) $ do
  payment  <- O.selectTable paymentTable
  customer <- O.selectTable customerTable
  where_ (pCustomerId payment .== cCustomerId customer)
  pure (cFirstName customer .++ O.sqlString " " .++ cLastName customer,
        pAmount payment)

exampleGroupBy_4 = O.aggregate ((,) <$> P.lmap pStaffId O.groupBy
                                    <*> P.lmap pPaymentDate O.count) $ do
  payment <- O.selectTable paymentTable
  pure payment

exampleGroupBy_5 =
  O.orderBy (O.asc (\(customerId, _, _) -> customerId)) $
  O.aggregate ((,,) <$> P.lmap pCustomerId O.groupBy
                                     <*> P.lmap pStaffId O.groupBy
                                     <*> P.lmap pAmount O.sum) $ do
  payment <- O.selectTable paymentTable
  pure payment

exampleGroupBy_6 = O.aggregate ((,) <$> P.lmap fst O.groupBy
                                    <*> P.lmap snd O.sum) $ do
  payment <- O.selectTable paymentTable
  pure (O.dateOfTimestamp (pPaymentDate payment), pAmount payment)

printNumberedRows :: Show a => [a] -> IO ()
printNumberedRows = mapM_ print . zip [1::Int ..]

main :: IO ()
main = withDvdRentalConnection $ \connchars conn -> do
  printNumberedRows =<< O.runSelectI conn example1
  printNumberedRows =<< O.runSelectI conn example2
  printNumberedRows =<< O.runSelectI conn example3
  printNumberedRows =<< O.runSelectI conn example4
  printNumberedRows =<< O.runSelectI conn example5
  printNumberedRows =<< O.runSelectI conn example3_1
  printNumberedRows =<< O.runSelectI conn example3_2
  printNumberedRows =<< O.runSelectI conn example3_3
  printNumberedRows =<< O.runSelectI conn example3_4
  printNumberedRows =<< O.runSelectI conn example3_5
  printNumberedRows =<< O.runSelectI conn example3_6
  printNumberedRows =<< O.runSelectI conn example3_7

  printNumberedRows =<< O.runSelectI conn exampleOrderBy_1
  printNumberedRows =<< O.runSelectI conn exampleOrderBy_2
  printNumberedRows =<< O.runSelectI conn exampleOrderBy_3
  printNumberedRows =<< O.runSelectI conn exampleOrderBy_4

  printNumberedRows =<< O.runSelectI conn exampleGroupBy_1
  printNumberedRows =<< O.runSelectI conn exampleGroupBy_2
  printNumberedRows =<< O.runSelectI conn exampleGroupBy_3
  printNumberedRows =<< O.runSelectI conn exampleGroupBy_4
  printNumberedRows =<< O.runSelectI conn exampleGroupBy_5
  printNumberedRows =<< O.runSelectI conn exampleGroupBy_6
  printNumberedRows =<< O.runSelectI conn salesByFilmCategoryView
  _ <- insertFilm conn
  printNumberedRows =<< O.runSelectI conn (O.selectTable filmTable)

withDvdRentalConnection :: Show r => (String -> PGS.Connection -> IO r) -> IO ()
withDvdRentalConnection f = do
  withDvdRentalConnectionString $ \connstr connchars -> do
    withConnectPostgreSQL connstr $ \conn -> do
      putStrLn "ready to interact with DB."

      f connchars conn

withDvdRentalConnectionString :: Show r
                              => (ByteString -> String -> IO r) -> IO ()
withDvdRentalConnectionString f = do
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

    f connstr connchars

  case e of
    Left l -> putStr "Error: " >> print l
    Right r -> putStr "Result: " >> print r

shDvdRentalConnection s = withDvdRentalConnectionString $ \_ -> sh s

sh s connchars = do
  putStrLn "You can access the connstr via $PGCONNSTR"
  putStrLn "For example: psql $PGCONNSTR"
  callProcess "sh" ["-c", "export PGCONNSTR=\'" ++ connchars ++ "\'; exec " ++ s]

where_ = O.viaLateral O.restrict

timestampOfString = O.unsafeCast "timestamp" . O.sqlString

withConnectPostgreSQL :: ByteString -> (PGS.Connection -> IO c) -> IO c
withConnectPostgreSQL connstr =
  bracket (PGS.connectPostgreSQL connstr) PGS.close

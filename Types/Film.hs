{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Film where

import qualified Opaleye as O

import           Opaleye.Experimental.Enum (fromFieldToFieldsEnum)
import           Opaleye.TypeFamilies (TableRecordField, Req, Opt, NN, W, O)
import           Opaleye.Internal.Inferrable

import qualified Data.Profunctor.Product.Default as D
import           Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)

-- { Implementing a Postgres ENUM type

-- Define a new type to represent the value on the SQL side
data SqlRating

-- Define a new sum type to represent all possible values of the ENUM
-- on the Haskell side
data Rating = G | PG | PG13 | R | NC17 deriving Show

-- Write a function that converts to the string representation of the ENUM
toSqlRatingString :: Rating -> String
toSqlRatingString r = case r of
    G    -> "G"
    PG   -> "PG"
    PG13 -> "PG-13"
    R    -> "R"
    NC17 -> "NC-17"

-- Write a function that converts from the string representation of the ENUM
fromSqlRatingString :: String -> Maybe Rating
fromSqlRatingString s = case s of
    "G"     -> Just G
    "PG"    -> Just PG
    "PG-13" -> Just PG13
    "R"     -> Just R
    "NC-17" -> Just NC17
    _       -> Nothing

-- Create the type class instance method implementations by providing
-- the mappers and the type name
(fromFieldRating, toFieldsRating) =
  fromFieldToFieldsEnum "mpaa_rating" fromSqlRatingString toSqlRatingString

-- Implement the typeclasses

instance O.DefaultFromField SqlRating Rating where
  defaultFromField = fromFieldRating

instance rating ~ Rating
  => D.Default (Inferrable O.FromFields) (O.Column SqlRating) rating where
  def = Inferrable D.def

instance D.Default O.ToFields Rating (O.Column SqlRating) where
  def = toFieldsRating

-- Finished implementing the ENUM type! }

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

type FilmR = Film' (O.Field O.SqlInt4)
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

type FilmW = Film' (Maybe (O.Field O.SqlInt4))
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

type FilmTF f = Film' (TableRecordField f () O.SqlInt4 NN Opt)
                      (TableRecordField f () O.SqlText NN Req)
                      (TableRecordField f () O.SqlText NN Req)
                      (TableRecordField f () O.SqlInt4 NN Req)
                      (TableRecordField f () O.SqlInt4 NN Req)
                      (TableRecordField f () O.SqlNumeric NN Req)
                      (TableRecordField f () O.SqlNumeric NN Req)
                      (TableRecordField f () O.SqlInt4 NN Req)
                      (TableRecordField f () O.SqlNumeric NN Req)
                      (TableRecordField f () SqlRating NN Req)
                      (TableRecordField f () O.SqlTimestamp NN Req)
                      (TableRecordField f () (O.SqlArray O.SqlText) NN Req)

$(makeAdaptorAndInstanceInferrable "pFilm" ''Film')

filmTable' :: O.Table (FilmTF W) (FilmTF O)
filmTable' = filmTable

filmTable :: O.Table FilmW FilmR
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

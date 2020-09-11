{-# OPTIONS_GHC -Wno-missing-signatures #-}

module OtherExamples.Insert where

import qualified Opaleye as O

import           Types.Film

example1 = O.Insert
  { O.iTable      = filmTable
  , O.iRows       = [ Film { fFilmId          = Nothing
                           , fTitle           = O.sqlString "My title"
                           , fDescription     = O.sqlString "My description"
                           , fReleaseYear     = 2001
                           , fLanguageId      = 1
                           , fRentalDuration  = 66
                           , fRentalRate      = 55
                           , fLength          = 44
                           , fReplacementCost = 33
                           , fRating          = O.toFields PG
                           , fLastUpdate      = Nothing
                           , fSpecialFeatures = O.sqlArray id []
                           }]
  , O.iReturning  = O.rReturningI fFilmId
  , O.iOnConflict = Nothing
  }

timestampOfString = O.unsafeCast "timestamp" . O.sqlString

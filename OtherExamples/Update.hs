{-# OPTIONS_GHC -Wno-missing-signatures #-}

module OtherExamples.Update where

import qualified Opaleye as O
import           Opaleye ((.++))

import           Types.Film

-- "Electric Boogaloo" has been voted the best movie sequel subtitle
-- so let's make all our films great by adding it to all of them!
--
-- https://www.ranker.com/list/funniest-movie-sequel-titles/carlybobarly
example1 = O.Update
  { O.uTable      = filmTable
  , O.uUpdateWith = O.updateEasy
      (\r -> r { fTitle = fTitle r
                 .++ O.sqlString " 2: Electric Boogaloo"
               , fDescription = fDescription r
                 .++ O.sqlString ", the hilarious sequel"
               })
  , O.uWhere = const (O.sqlBool True)
  , O.uReturning = O.rCount
  }

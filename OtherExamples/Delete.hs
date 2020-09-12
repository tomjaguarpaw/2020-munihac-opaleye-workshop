{-# OPTIONS_GHC -Wno-missing-signatures #-}

module OtherExamples.Delete where

import qualified Opaleye as O
import           Opaleye ((.==), (.&&))

import           Types.FilmActor
import           Types.Actor

example1 conn = do
  [ianTandyId] <- O.runSelectI conn $ do
    a  <- O.selectTable actorTable
    O.where_ (aFirstName a .== O.sqlString "Ian"
             .&& aLastName a .== O.sqlString "Tandy")
    pure (aActorId a)

  O.runDelete_ conn O.Delete
    { O.dTable      = filmActorTable
    , O.dWhere      = \r -> faActorId r .== O.sqlInt4 ianTandyId
    , O.dReturning  = O.rCount
    }

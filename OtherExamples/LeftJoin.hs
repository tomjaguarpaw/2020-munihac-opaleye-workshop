{-# OPTIONS_GHC -Wno-missing-signatures #-}

module OtherExamples.LeftJoin where

import Types.Film
import Types.Inventory

import Opaleye as O
import Opaleye.Join as OJ

example1 = do
  film <- O.selectTable filmTable
  minventoryId <- OJ.optional $ do
    inventory <- O.selectTable inventoryTable
    O.where_ (fFilmId film .== pFilmId inventory)
    pure (pInventoryId inventory)

  pure (fFilmId film, fTitle film, minventoryId)

example2 :: O.Select (FilmR, InventoryNullable)
example2 =
  O.leftJoin (O.selectTable filmTable)
             (O.selectTable inventoryTable)
             (\(film, inventory) ->
                fFilmId film .== pFilmId inventory)

example3 = do
  (film, inventory) <- example2
  O.where_ (fTitle film .== O.sqlString "Alice Fantasia")
  pure (fFilmId film, fTitle film, pInventoryId inventory)

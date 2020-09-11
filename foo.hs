module Main where

import           Types.Film

import           Examples.GroupBy
import           Examples.OrderBy
import           Examples.Select
import           Examples.Where
import           OtherExamples.Insert
import           OtherExamples.View

import           Connectivity (withDvdRentalConnection)

import qualified Opaleye as O
import           Opaleye ((.>), (.===))

-- Run this first to check that everything is set up correctly
--
-- Things that could go wrong:
--
-- * Opaleye hasn't been installed and made available to the REPL
--
-- * Postgres isn't installed (initdb needs to be on your PATH)
--
-- * The dvdrental database is not available (get it from
--   https://www.postgresqltutorial.com/postgresql-sample-database/)
--
-- * The dvdrental database cannot be found.  Tweak
--   'Connectivity.tarFile' to point to it.
smokeTest :: IO ()
smokeTest = do
  withDvdRentalConnection $ \conn -> do
    works <- O.runSelectI conn $ do
      r <- O.selectTable filmTable
      O.where_ (fFilmId r .=== 999)
      pure (fTitle r .=== O.sqlString "Zoolander Fiction")
    putStrLn $ case works == [True] of
      True  -> "smoke test was successful"
      False -> "SMOKE TEST FAILED!"

filmSelect :: O.Select FilmR
filmSelect = do
  film <- O.selectTable filmTable
  O.where_ (fFilmId film .> 990)
  pure film

printNumberedRows :: Show a => [a] -> IO ()
printNumberedRows = mapM_ print . zip [1::Int ..]

main :: IO ()
main = withDvdRentalConnection $ \conn -> do
  printNumberedRows =<< O.runSelectI conn Examples.Select.example1
  printNumberedRows =<< O.runSelectI conn Examples.Select.example2
  printNumberedRows =<< O.runSelectI conn Examples.Select.example3
  printNumberedRows =<< O.runSelectI conn Examples.Select.example4
  printNumberedRows =<< O.runSelectI conn Examples.Select.example5

  printNumberedRows =<< O.runSelectI conn Examples.Where.example1
  printNumberedRows =<< O.runSelectI conn Examples.Where.example2
  printNumberedRows =<< O.runSelectI conn Examples.Where.example3
  printNumberedRows =<< O.runSelectI conn Examples.Where.example4
  printNumberedRows =<< O.runSelectI conn Examples.Where.example5
  printNumberedRows =<< O.runSelectI conn Examples.Where.example6
  printNumberedRows =<< O.runSelectI conn Examples.Where.example7

  printNumberedRows =<< O.runSelectI conn Examples.OrderBy.example1
  printNumberedRows =<< O.runSelectI conn Examples.OrderBy.example2
  printNumberedRows =<< O.runSelectI conn Examples.OrderBy.example3
  printNumberedRows =<< O.runSelectI conn Examples.OrderBy.example4

  printNumberedRows =<< O.runSelectI conn Examples.GroupBy.example1
  printNumberedRows =<< O.runSelectI conn Examples.GroupBy.example2
  printNumberedRows =<< O.runSelectI conn Examples.GroupBy.example3
  printNumberedRows =<< O.runSelectI conn Examples.GroupBy.example4
  printNumberedRows =<< O.runSelectI conn Examples.GroupBy.example5
  printNumberedRows =<< O.runSelectI conn Examples.GroupBy.example6
  printNumberedRows =<< O.runSelectI conn salesByFilmCategoryView
  printNumberedRows =<< O.runInsert_ conn OtherExamples.Insert.example1
  printNumberedRows =<< O.runSelectI conn filmSelect

module OtherExamples.View where

import qualified Opaleye as O

import           Types.Film

import qualified Data.Profunctor.Product as PP

-- To make a view you just make a Table and then select from it
salesByFilmCategoryView :: O.Select (O.Field O.SqlText, O.Field O.SqlNumeric)
salesByFilmCategoryView =
  O.selectTable (O.table "sales_by_film_category"
                   (PP.p2 (O.requiredTableField "category",
                           O.requiredTableField "total_sales")))

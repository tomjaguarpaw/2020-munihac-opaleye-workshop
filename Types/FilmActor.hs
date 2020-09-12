{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.FilmActor where

import qualified Opaleye as O

import           Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)

data FilmActor' a b c = FilmActor
  { faFilmId     :: a
  , faActorId    :: b
  , faLastUpdate :: c
  }
  deriving Show

type FilmActorR = FilmActor' (O.Field O.SqlInt4)
                             (O.Field O.SqlInt4)
                             (O.Field O.SqlTimestamp)

$(makeAdaptorAndInstanceInferrable "pFilmActor" ''FilmActor')

filmActorTable :: O.Table FilmActorR FilmActorR
filmActorTable = O.table "film_actor" (pFilmActor (FilmActor
    { faFilmId     = O.tableField "film_id"
    , faActorId    = O.tableField "actor_id"
    , faLastUpdate = O.tableField "last_update"
    }))

{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Actor where

import qualified Opaleye as O

import           Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)

data Actor' a b c d = Actor
  { aActorId    :: a
  , aFirstName  :: b
  , aLastName   :: c
  , aLastUpdate :: d
  }
  deriving Show

type ActorR = Actor' (O.Field O.SqlInt4)
                     (O.Field O.SqlText)
                     (O.Field O.SqlText)
                     (O.Field O.SqlTimestamp)

$(makeAdaptorAndInstanceInferrable "pActor" ''Actor')

actorTable :: O.Table ActorR ActorR
actorTable = O.table "actor" (pActor (Actor
    { aActorId    = O.tableField "actor_id"
    , aFirstName  = O.tableField "first_name"
    , aLastName   = O.tableField "last_name"
    , aLastUpdate = O.tableField "last_update"
    }))

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.Depot where

import Opaleye
import Control.Monad (mzero)
import Data.Aeson
import Data.DateTime (DateTime)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import App

data Depot' a b c d e =
  Depot { dpId        :: a
        , dpShelfId   :: b
        , dpItemId    :: c
        , dpQuantity  :: d
        , dpTimestamp :: e
        }

type DepotRead  = Depot' DepotID         ShelfID ItemID Int DateTime
type DepotWrite = Depot' (Maybe DepotID) ShelfID ItemID Int (Maybe DateTime)
-- type DepotRead  = Depot' DepotID         Int DateTime
-- type DepotWrite = Depot' (Maybe DepotID) Int (Maybe DateTime)

type DepotColumnRead = Depot' (Column PGInt8)
                              (Column PGInt8)
                              (Column PGInt8)
                              (Column PGInt4)
                              (Column PGTimestamptz)

type DepotColumnWrite = Depot' (Maybe (Column PGInt8))
                                      (Column PGInt8)
                                      (Column PGInt8)
                                      (Column PGInt4)
                               (Maybe (Column PGTimestamptz))

instance ToJSON DepotRead where
  toJSON depot = object [ "id"        .= dpId depot
                        , "shelf_id"  .= dpShelfId depot
                        , "item_id"   .= dpItemId depot
                        , "quantity"  .= dpQuantity depot
                        , "timestamp" .= dpTimestamp depot
                        ]

instance FromJSON DepotWrite where
  parseJSON (Object o) = Depot     <$>
                 o .:? "id"        <*>
                 o .:  "shelf_id"  <*>
                 o .:  "item_id"   <*>
                 o .:  "quantity"  <*>
                 o .:? "timestamp"
  parseJSON _ = mzero


$(makeAdaptorAndInstance "pDepot" ''Depot')

depotTable :: Table DepotColumnWrite DepotColumnRead
depotTable =  Table "shelfitems" (pDepot Depot { dpId        = optional "id"
                                           -- , dpShelfId   = pShelfId . ShelfId $ required "shelf_id"
                                           -- , dpItemId    = pItemId . ItemId $ required "item_id"
                                           , dpShelfId   = required "shelf_id"
                                           , dpItemId    = required "item_id"
                                           , dpQuantity  = required "quantity"
                                           , dpTimestamp = optional "timestamp"
                                           })

depotToPG :: DepotWrite -> DepotColumnWrite
depotToPG = pDepot Depot { dpId         = const Nothing
                         , dpShelfId    = pgInt8
                         , dpItemId     = pgInt8
                         , dpQuantity   = pgInt4
                         , dpTimestamp  = const Nothing
                         }

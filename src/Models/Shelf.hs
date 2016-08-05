{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.Shelf where

import Opaleye
import Control.Monad (mzero)
import Data.Aeson
import Data.DateTime (DateTime)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import App

data Shelf' id label size time = Shelf
                                   { shId         :: id
                                   , shLabel      :: label
                                   , shSize       :: size
                                   , shTimestamp  :: time
                                   }

type ShelfRead  = Shelf' ShelfID String Int DateTime
type ShelfWrite = Shelf' (Maybe ShelfID) String Int (Maybe DateTime)

type ShelfColumnRead = Shelf' (Column PGInt8)
                              (Column PGText)
                              (Column PGInt4)
                              (Column PGTimestamptz)

type ShelfColumnWrite = Shelf' (Maybe (Column PGInt8))
                                      (Column PGText)
                                      (Column PGInt4)
                               (Maybe (Column PGTimestamptz))

instance ToJSON ShelfRead where
  toJSON shelf = object [ "id"        .= shId shelf
                        , "label"     .= shLabel shelf
                        , "size"      .= shSize shelf
                        , "timestamp" .= shTimestamp shelf
                        ]

instance FromJSON ShelfWrite where
  parseJSON (Object o) = Shelf <$>
                 o .:? "id"    <*>
                 o .:  "label" <*>
                 o .:  "size"  <*>
                 o .:? "timestamp"
  parseJSON _ = mzero

$(makeAdaptorAndInstance "pShelf" ''Shelf')

shelfTable :: Table ShelfColumnWrite ShelfColumnRead
shelfTable =  Table "shelfs" (pShelf Shelf { shId         = optional "id"
                                           , shLabel      = required "label"
                                           , shSize       = required "size"
                                           , shTimestamp  = optional "timestamp"
                                           })

shelfToPG :: ShelfWrite -> ShelfColumnWrite
shelfToPG = pShelf Shelf { shId         = const Nothing
                         , shLabel      = pgString
                         , shSize       = pgInt4
                         , shTimestamp  = const Nothing
                         }

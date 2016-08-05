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

data Shelf' id title body email time = Shelf
                                        { shId         :: id
                                        , shTitle      :: title
                                        , shBody       :: body
                                        , shUsersEmail :: email
                                        , shTimestamp  :: time
                                        }

type ShelfRead  = Shelf' ShelfID String String Email DateTime
type ShelfWrite = Shelf' (Maybe ShelfID) String String Email (Maybe DateTime)

type ShelfColumnRead = Shelf' (Column PGInt8)
                              (Column PGText)
                              (Column PGText)
                              (Column PGText)
                              (Column PGTimestamptz)

type ShelfColumnWrite = Shelf' (Maybe (Column PGInt8))
                                      (Column PGText)
                                      (Column PGText)
                                      (Column PGText)
                               (Maybe (Column PGTimestamptz))

instance ToJSON ShelfRead where
  toJSON shelf = object [ "id"        .= shId shelf
                        , "title"     .= shTitle shelf
                        , "body"      .= shBody shelf
                        , "email"     .= shUsersEmail shelf
                        , "timestamp" .= shTimestamp shelf
                        ]

instance FromJSON ShelfWrite where
  parseJSON (Object o) = Shelf <$>
                 o .:? "id"    <*>
                 o .:  "title" <*>
                 o .:  "body"  <*>
                 o .:  "email" <*>
                 o .:? "timestamp"
  parseJSON _ = mzero

$(makeAdaptorAndInstance "pShelf" ''Shelf')

shelfTable :: Table ShelfColumnWrite ShelfColumnRead
shelfTable =  Table "shelfs" (pShelf Shelf { shId         = optional "id"
                                           , shTitle      = required "title"
                                           , shBody       = required "body"
                                           , shUsersEmail = required "users_email"
                                           , shTimestamp  = optional "timestamp"
                                           })

shelfToPG :: ShelfWrite -> ShelfColumnWrite
shelfToPG = pShelf Shelf { shId         = const Nothing
                         , shTitle      = pgString
                         , shBody       = pgString
                         , shUsersEmail = pgString
                         , shTimestamp  = const Nothing
                         }

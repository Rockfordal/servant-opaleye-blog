{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Models.Item where

import Opaleye
import Control.Monad (mzero)
import Data.Aeson
import Data.DateTime (DateTime)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import App
import GHC.Generics
import Data.Typeable

data Item' a b c d =
  Item
    { itId        :: a
    , itName      :: b
    , itInfo      :: c
    , itTimestamp :: d
    } deriving (Show, Generic, Typeable)

type ItemRead  = Item' ItemID         String String DateTime
type ItemWrite = Item' (Maybe ItemID) String String (Maybe DateTime)

type ItemColumnRead = Item' (Column PGInt8)
                            (Column PGText)
                            (Column PGText)
                            (Column PGTimestamptz)

type ItemColumnWrite = Item' (Maybe (Column PGInt8))
                                    (Column PGText)
                                    (Column PGText)
                             (Maybe (Column PGTimestamptz))

instance ToJSON ItemRead where
  toJSON item = object [ "id"        .= itId        item
                       , "name"      .= itName      item
                       , "info"      .= itInfo      item
                       , "timestamp" .= itTimestamp item
                       ]

instance FromJSON ItemWrite where
  parseJSON (Object o) = Item      <$>
                 o .:? "id"        <*>
                 o .:  "name"      <*>
                 o .:  "info"      <*>
                 o .:? "timestamp"
  parseJSON _ = mzero

$(makeAdaptorAndInstance "pItem" ''Item')

itemTable :: Table ItemColumnWrite ItemColumnRead
itemTable =  Table "items" (pItem Item { itId        = optional "id"
                                       , itName      = required "name"
                                       , itInfo      = required "info"
                                       , itTimestamp = optional "timestamp"
                                       })

itemToPG :: ItemWrite -> ItemColumnWrite
itemToPG = pItem Item { itId         = const Nothing
                      , itName       = pgString
                      , itInfo       = pgString
                      , itTimestamp  = const Nothing
                      }

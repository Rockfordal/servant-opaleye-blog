{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.Product where

import Opaleye
import Control.Monad (mzero)
import Data.Aeson
import Data.DateTime (DateTime)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import App

data Product' a b c d = Product
        { prId        :: a
        , prShelfId   :: b
        , prName      :: c
        , prTimestamp :: d
        }

type ProductRead  = Product' ProductID         ShelfID String DateTime
type ProductWrite = Product' (Maybe ProductID) ShelfID String (Maybe DateTime)

type ProductColumnRead = Product' (Column PGInt8)
                                  (Column PGInt8)
                                  (Column PGText)
                                  (Column PGTimestamptz)

type ProductColumnWrite = Product' (Maybe (Column PGInt8))
                                   (Column PGInt8)
                                   (Column PGText)
                            (Maybe (Column PGTimestamptz))

instance ToJSON ProductRead where
  toJSON product = object [ "id"        .= prId product
                          , "shelf_id"  .= prShelfId product
                          , "name"      .= prName product
                          , "timestamp" .= prTimestamp product
                          ]

instance FromJSON ProductWrite where
  parseJSON (Object o) = Product      <$>
                    o .:? "id"        <*>
                    o .:  "shelf_id"  <*>
                    o .:  "name"      <*>
                    o .:? "timestamp"
  parseJSON _ = mzero

$(makeAdaptorAndInstance "pProduct" ''Product')

productTable :: Table ProductColumnWrite ProductColumnRead
productTable =  Table "products" (pProduct Product { prId        = optional "id"
                                                   , prShelfId   = required "shelf_id"
                                                   , prName      = required "name"
                                                   , prTimestamp = optional "timestamp"
                                                   })

productToPG :: ProductWrite -> ProductColumnWrite
productToPG = pProduct Product { prId        = const Nothing
                               , prShelfId   = pgInt8
                               , prName      = pgString
                               , prTimestamp = const Nothing
                               }

{-# LANGUAGE Arrows #-}

module Queries.Item where

import Opaleye
import Control.Arrow (returnA)

import App
import Models.Item
import Models.Shelf
import Models.Depot

itemsQuery :: Query ItemColumnRead
itemsQuery = queryTable itemTable

itemByIdQuery :: ItemID -> Query ItemColumnRead
itemByIdQuery id = proc () -> do
                        item     <- itemsQuery -< ()
                        restrict -< itId item .== pgInt8 id
                        returnA  -< item

{-# LANGUAGE Arrows #-}

module Queries.Item where

import Opaleye
import Control.Arrow (returnA)

import App
import Models.Item

itemQuery :: Query ItemColumnRead
itemQuery = queryTable itemTable

itemByIdQuery :: ItemID -> Query ItemColumnRead
itemByIdQuery id = proc () -> do
                        item     <- itemQuery -< ()
                        restrict -< itId item .== pgInt8 id
                        returnA  -< item

-- itemsBySizeQuery :: Size -> Query ItemColumnRead
-- itemsBySizeQuery size = proc () -> do
--                                item     <- itemQuery -< ()
--                                restrict -< itSize item .== pgString size
--                                returnA  -< item

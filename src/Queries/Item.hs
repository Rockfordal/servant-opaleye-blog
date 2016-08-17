{-# LANGUAGE Arrows #-}
module Queries.Item where

import Opaleye
import Control.Arrow (returnA)

import App
import Models.Item


itemsQuery :: Query ItemColumnRead
itemsQuery = queryTable itemTable

itemByIdQuery :: ItemID -> Query ItemColumnRead
itemByIdQuery idToMatch = proc () -> do
                        item     <- itemsQuery -< ()
                        restrict -< itId item .== pgInt8 idToMatch
                        returnA  -< item

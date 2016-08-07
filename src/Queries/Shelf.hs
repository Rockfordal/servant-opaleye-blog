{-# LANGUAGE Arrows #-}

module Queries.Shelf where

import Opaleye
import Control.Arrow (returnA)

import App
import Models.Shelf
import Models.Item
import Models.Depot


shelfsQuery :: Query ShelfColumnRead
shelfsQuery = queryTable shelfTable

shelfByIdQuery :: ShelfID -> Query ShelfColumnRead
shelfByIdQuery id = proc () -> do
                        shelf     <- shelfsQuery -< ()
                        restrict -< shId shelf .== pgInt8 id
                        returnA  -< shelf

shelfsBySizeQuery :: ShelfSize -> Query ShelfColumnRead
shelfsBySizeQuery size = proc () -> do
                               shelf     <- shelfsQuery -< ()
                               restrict -< shSize shelf .== pgInt4 size
                               returnA  -< shelf

shelfsByLabelQuery :: ShelfLabel -> Query ShelfColumnRead
shelfsByLabelQuery name = proc () -> do
  shelf     <- shelfsQuery -< ()
  restrict -< shLabel shelf .== pgString name
  returnA  -< shelf

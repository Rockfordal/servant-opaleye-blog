{-# LANGUAGE Arrows #-}

module Queries.Shelf where

import Opaleye
import Control.Arrow (returnA)

import App
import Models.Shelf

shelfQuery :: Query ShelfColumnRead
shelfQuery = queryTable shelfTable

shelfByIdQuery :: ShelfID -> Query ShelfColumnRead
shelfByIdQuery id = proc () -> do
                        shelf     <- shelfQuery -< ()
                        restrict -< shId shelf .== pgInt8 id
                        returnA  -< shelf

shelfsByEmailQuery :: Email -> Query ShelfColumnRead
shelfsByEmailQuery email = proc () -> do
                               shelf     <- shelfQuery -< ()
                               restrict -< shUsersEmail shelf .== pgString email
                               returnA  -< shelf

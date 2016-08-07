{-# LANGUAGE Arrows #-}

module Queries.Item where

import Opaleye
import Control.Arrow (returnA)
-- import Control.Lens (^.) -- (makeLenses,(^.),(.~),(%~),to,_1)
import Queries.Shelf
import Queries.Depot

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

-- shQuery :: Query (Column PGInt8, Column PGText, Column PGText, Column PGInt4, Column PGTimestamptz)
-- shQuery = queryTable shelfTable

-- booksWithKeywordQuery :: Column PGText -> Query BookColumns
-- booksWithKeywordQuery kw = proc () -> do
--   b <- bookQuery -< ()
--   bookRestrictedByKeyword kw -< b
--   returnA -< b

-- booksWithKeywordQuery1 :: Column PgText -> Query BookColumns
-- booksWithKeywordQuery1 kw = proc () -> do
--   b <- bookQuery        -< ()
--   k <- bookKeywordQuery -< ()
--   restrict -< b^.bookIsbn.to unIsbn .== k^bookKeywordBookIsbn.to unIsbn
--   restrit -< k^bookKeywordKeyword .== kw
--   return -< b

-- bookKeywordJoin :: QueryArr BookColumns (Column PGText)
-- bookKeywordJoin = proc (b) -> do
--   k <- bookKeywordQuery -< ()
--   restrict -< b^.bookIsbn.to unIsbn .== k^.bookKeywordBookIsbn.to unIsbn
--   returnA -< k^.bookKeywordKeyword

-- itemsByShelfNameQuery :: ShelfName -> Query ShelfColumnRead
-- itemsByShelfNameQuery name = proc () -> do
--   shelf     <- shelfsQuery -< ()
--   restrict -< shLabel shelf .== pgString name
--   returnA  -< shelf

-- itemsByShelfIdQuery :: ShelfID   -> Query (Column PGInt8, Column PGInt8, Column PGInt8, Column PGInt4, Column PGTimestamptz)

itemsByShelfIdQuery :: ShelfID -> Query DepotColumnRead
itemsByShelfIdQuery shelfid = proc () -> do
                                depot     <- depotsQuery -< ()
                                restrict -< dpShelfId depot .== pgInt8 shelfid
                                returnA  -< depot

-- itemsByShelfLabelQuery :: ShelfLabel -> Query DepotColumnRead
-- itemsByShelfLabelQuery label = proc () -> do
--                                 depot     <- depotsQuery -< ()
--                                 shelf     <- shelfsQuery -< ()
--                                 item      <- itemsQuery  -< ()
--                                 restrict -< dpShelfId depot .== shId shelf
--                                 restrict -< dpItemId depot  .== itId item
--                                 restrict -< shLabel shelf   .== pgString label
--                                 returnA  -< depot

itemsByShelfLabelQuery :: ShelfLabel -> Query ItemColumnRead
itemsByShelfLabelQuery label = proc () -> do
                                depot     <- depotsQuery -< ()
                                shelf     <- shelfsQuery -< ()
                                item      <- itemsQuery  -< ()
                                restrict -< dpShelfId depot .== shId shelf
                                restrict -< dpItemId depot  .== itId item
                                restrict -< shLabel shelf   .== pgString label
                                returnA  -< item

                                -- (id, label, position, size, time) <- shelfsQuery -< ()
                                -- restrict   -< label .== pgString name
                                -- returnA    -< (position shelf)


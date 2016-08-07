{-# LANGUAGE Arrows #-}

module Queries.Item where

import Opaleye
import Control.Arrow (returnA)
-- import Control.Lens (^.) -- (makeLenses,(^.),(.~),(%~),to,_1)
import Queries.Shelf

import App
import Models.Item
import Models.Shelf

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

-- bookKeywordJoin :: QueryArr BookColumns (Column PGText)
-- bookKeywordJoin = proc (b) -> do
--   k <- bookKeywordQuery -< ()
--   restrict -< b^.bookIsbn.to unIsbn .== k^.bookKeywordBookIsbn.to unIsbn
--   returnA -< k^.bookKeywordKeyword

itemsByShelfNameQuery :: ShelfName -> Query ShelfColumnRead
itemsByShelfNameQuery name = proc () -> do
  shelf     <- shelfsQuery -< ()
  restrict -< shLabel shelf .== pgString name
  returnA  -< shelf

-- itemsByShelfNameQuery :: ShelfName -> Query (Column PGText, Column PGText)
-- itemsByShelfNameQuery :: ShelfName -> Query (Column PGInt8, Column PGText, Column PGText, Column PGInt4, Column PGTimestamptz)

-- itemsByShelfNameQuery name = proc () -> do
--                                 shelf     <- shelfsQuery -< ()

                                -- (id, label, position, size, time) <- shelfsQuery -< ()
                                -- restrict   -< shLabel shelf .== pgString name
                                -- restrict   -< label .== pgString name
                                -- returnA    -< shelf
                                -- returnA    -< (position shelf)


-- itemsBySizeQuery :: ItemSize -> Query ItemColumnRead
-- itemsBySizeQuery size = proc () -> do
--                                item     <- itemsQuery -< ()
--                                restrict -< itSize item .== pgString size
--                                returnA  -< item


-- booksWithKeywordQuery :: Column PgText -> Query BookColumns
-- booksWithKeywordQuery kw = proc () -> do
--   b <- bookQuery        -< ()
--   k <- bookKeywordQuery -< ()
--   restrict -< b^.bookIsbn.to unIsbn .== k^bookKeywordBookIsbn.to unIsbn
--   restrit -< k^bookKeywordKeyword .== kw
--   return -< b

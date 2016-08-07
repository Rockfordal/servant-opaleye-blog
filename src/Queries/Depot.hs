{-# LANGUAGE Arrows #-}

module Queries.Depot where

import Opaleye
import Control.Arrow (returnA)
-- import Control.Lens (^.) -- (makeLenses,(^.),(.~),(%~),to,_1)

import App
import Models.Depot
import Models.Shelf
import Models.Item
import Queries.Shelf
import Queries.Item

depotsQuery :: Query DepotColumnRead
depotsQuery = queryTable depotTable

itemsByShelfLabelQuery :: ShelfLabel -> Query ItemColumnRead
itemsByShelfLabelQuery label = proc () -> do
                                depot     <- depotsQuery -< ()
                                shelf     <- shelfsQuery -< ()
                                item      <- itemsQuery  -< ()
                                restrict -< dpShelfId depot .== shId shelf
                                restrict -< dpItemId depot  .== itId item
                                restrict -< shLabel shelf   .== pgString label
                                returnA  -< item

shelfsByItemIdQuery :: ItemID -> Query ShelfColumnRead
shelfsByItemIdQuery itemid = proc () -> do
                                depot     <- depotsQuery -< ()
                                shelf     <- shelfsQuery -< ()
                                item      <- itemsQuery  -< ()
                                restrict -< dpShelfId depot .== shId shelf
                                restrict -< dpItemId depot  .== itId item
                                restrict -< itId item       .== pgInt8 itemid
                                returnA  -< shelf

itemsByShelfIdQuery :: ShelfID -> Query DepotColumnRead
itemsByShelfIdQuery shelfid = proc () -> do
                                depot     <- depotsQuery -< ()
                                restrict -< dpShelfId depot .== pgInt8 shelfid
                                returnA  -< depot

                                -- (id, label, position, size, time) <- shelfsQuery -< ()
                                -- returnA -< (position shelf)


-- depotOrphansLeftJoin :: Query ((Column PGText, Column PGInt4, Column PGText),
--                                 ColumnNullableBirthday)
-- depotOrphansLeftJOin = leftJoin personQuery birthdayQuery eqName
--      where eqName ((name, _, _), birthdayRow) = name .== bdName birthdayRow


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

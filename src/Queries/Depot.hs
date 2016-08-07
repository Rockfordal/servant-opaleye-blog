{-# LANGUAGE Arrows #-}

module Queries.Depot where

import Opaleye
import Control.Arrow (returnA)

import App
import Models.Depot

depotsQuery :: Query DepotColumnRead
depotsQuery = queryTable depotTable

-- depotByIdQuery :: DepotID -> Query DepotColumnRead
-- depotByIdQuery id = proc () -> do
--                         depot     <- depotsQuery -< ()
--                         restrict -< shId depot .== pgInt8 id
--                         returnA  -< depot

-- depotsBySizeQuery :: DepotSize -> Query DepotColumnRead
-- depotsBySizeQuery size = proc () -> do
--                           depot     <- depotsQuery -< ()
--                           restrict -< shSize depot .== pgInt4 size
--                           returnA  -< depot

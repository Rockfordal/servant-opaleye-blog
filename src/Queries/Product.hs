{-# LANGUAGE Arrows #-}

module Queries.Product where

import Opaleye
import Control.Arrow (returnA)

import App
import Models.Product


productsQuery :: Query ProductColumnRead
productsQuery = queryTable productTable

productByIdQuery :: ProductID -> Query ProductColumnRead
productByIdQuery prodid = proc () -> do
                    prod      <- productsQuery -< ()
                    restrict -<  prId prod .== pgInt8 prodid
                    returnA  -<  prod

productsByNameQuery :: ProductName -> Query ProductColumnRead
productsByNameQuery name = proc () -> do
                    prod      <- productsQuery -< ()
                    restrict -<  prName prod .== pgString name
                    returnA  -<  prod

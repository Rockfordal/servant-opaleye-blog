{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Depot where

import Servant
import Opaleye
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (listToMaybe)
import Data.Int (Int64)

import App
import Models.Depot
import Queries.Depot

type DepotAPI = Get '[JSON] [DepotRead]
           -- :<|> Capture "id"              DepotID    :> Get  '[JSON] (Maybe DepotRead)
           -- :<|> "size" :> Capture "size"  DepotSize  :> Get  '[JSON] [DepotRead]
           :<|> ReqBody '[JSON]           DepotWrite :> Post '[JSON] Int64

depotAPI :: Proxy DepotAPI
depotAPI = Proxy

depotServer :: ServerT DepotAPI AppM
depotServer = getDepots
            -- :<|> getDepotById
            -- :<|> getDepotsBySize
            :<|> depotPost

getDepots :: AppM [DepotRead]
getDepots = do
  con <- ask
  liftIO $ runQuery con depotsQuery

-- getItemsByShelfId :: ShelfID -> AppM (Maybe ItemRead)
-- getItemsByShelfId shelfid = do
--   con <- ask
--   liftIO $ listToMaybe <$> runQuery con (itemsByShelfIdQuery shelfid)

-- getDepotById :: DepotID -> AppM (Maybe DepotRead)
-- getDepotById id = do
--   con <- ask
--   liftIO $ listToMaybe <$> runQuery con (depotByIdQuery id)

-- getDepotsBySize :: DepotSize -> AppM [DepotRead]
-- getDepotsBySize size = do
--   con <- ask
--   liftIO $ runQuery con (depotsBySizeQuery size)

depotPost :: DepotWrite -> AppM Int64
depotPost depot = do
  con <- ask
  liftIO $ runInsert con depotTable $ depotToPG depot

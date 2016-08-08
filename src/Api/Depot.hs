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

type DepotAPI =                           Get    '[JSON] [DepotRead]
       :<|> Capture "id" DepotID       :> Get    '[JSON] (Maybe DepotRead)
       :<|> ReqBody '[JSON] DepotWrite :> Post   '[JSON] Int64
       :<|> Capture "id"    DepotID    :> Delete '[JSON] Int64

depotAPI :: Proxy DepotAPI
depotAPI = Proxy

depotServer :: ServerT DepotAPI AppM
depotServer = getDepots
            :<|> getDepotById
            :<|> postDepot
            :<|> deleteDepot

getDepots :: AppM [DepotRead]
getDepots = do
  con <- ask
  liftIO $ runQuery con depotsQuery

getDepotById :: DepotID -> AppM (Maybe DepotRead)
getDepotById id = do
  con <- ask
  liftIO $ listToMaybe <$> runQuery con (depotByIdQuery id)

postDepot :: DepotWrite -> AppM Int64
postDepot depot = do
  con <- ask
  liftIO $ runInsert con depotTable $ depotToPG depot

deleteDepot :: DepotID -> AppM Int64
deleteDepot idToMatch = do
  con <- ask
  liftIO $ runDelete con depotTable match
  where
    match = (\d -> (dpId d) .=== (pgInt8 idToMatch))

-- getOrphanDepots :: AppM [DepotRead]
-- getOrphanDepots = do
--   con <- ask
--   liftIO $ runQuery con depotsByOphan

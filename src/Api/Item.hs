{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Item where

import Servant
import Opaleye
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (listToMaybe)
import Data.Int (Int64)

import App
import Models.Shelf
import Models.Item
import Models.Depot
import Queries.Item
import Queries.Depot

type ItemAPI =                                          Get  '[JSON] [ItemRead]
          :<|>            Capture "id" ItemID        :> Get  '[JSON] (Maybe ItemRead)
          :<|> "shelf" :> Capture "id" ShelfID       :> Get  '[JSON] (Maybe DepotRead)
          :<|> "shelf" :> Capture "label" ShelfLabel :> Get  '[JSON] [ItemRead]
          :<|>            ReqBody '[JSON] ItemWrite  :> Post '[JSON] Int64

itemAPI :: Proxy ItemAPI
itemAPI = Proxy

itemServer :: ServerT ItemAPI AppM
itemServer = getItems
            :<|> getItemById
            :<|> getItemsByShelfId
            :<|> getItemsByShelfLabel
            :<|> itemPost

getItems :: AppM [ItemRead]
getItems = do
  con <- ask
  liftIO $ runQuery con itemsQuery

getItemById :: ItemID -> AppM (Maybe ItemRead)
getItemById id = do
  con <- ask
  liftIO $ listToMaybe <$> runQuery con (itemByIdQuery id)

getItemsByShelfId :: ShelfID -> AppM (Maybe DepotRead)
getItemsByShelfId shelfid = do
  con <- ask
  liftIO $ listToMaybe <$> runQuery con (itemsByShelfIdQuery shelfid)

getItemsByShelfLabel :: ShelfLabel -> AppM [ItemRead]
getItemsByShelfLabel label = do
  con <- ask
  liftIO $ runQuery con (itemsByShelfLabelQuery label)

itemPost :: ItemWrite -> AppM Int64
itemPost item = do
  con <- ask
  liftIO $ runInsert con itemTable $ itemToPG item

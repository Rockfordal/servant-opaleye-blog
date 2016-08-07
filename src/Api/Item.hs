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
import Queries.Item

type ItemAPI = Get '[JSON] [ItemRead]
              :<|> Capture "id"        ItemID    :> Get  '[JSON] (Maybe ItemRead)
              :<|> Capture "shelfname" ShelfName :> Get  '[JSON] [ShelfRead]
              :<|> ReqBody '[JSON]     ItemWrite :> Post '[JSON] Int64

itemAPI :: Proxy ItemAPI
itemAPI = Proxy

itemServer :: ServerT ItemAPI AppM
itemServer = getItems
            :<|> getItemById
            :<|> getItemsByShelfname
            :<|> itemPost


getItems :: AppM [ItemRead]
getItems = do
  con <- ask
  liftIO $ runQuery con itemsQuery

getItemById :: ItemID -> AppM (Maybe ItemRead)
getItemById id = do
  con <- ask
  liftIO $ listToMaybe <$> runQuery con (itemByIdQuery id)

getItemsByShelfname :: ShelfName -> AppM [ShelfRead]
getItemsByShelfname name = do
  con <- ask
  liftIO $ runQuery con (itemsByShelfNameQuery name)
-- itemsByShelfNameQuery :: ShelfName -> Query ShelfColumnRead

itemPost :: ItemWrite -> AppM Int64
itemPost item = do
  con <- ask
  liftIO $ runInsert con itemTable $ itemToPG item

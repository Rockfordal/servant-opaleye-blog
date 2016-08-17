{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api.Item where

import Servant
import Servant.Server.Internal.ServantErr
import Opaleye
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Except
import Data.Maybe (listToMaybe)
import Data.Int (Int64)

import App
import Models.Item
import Models.Depot
import Queries.Item
import Queries.Depot


type ItemAPI =                                          Get    '[JSON] [ItemRead]
          :<|>            Capture "id"    ItemID     :> Get    '[JSON] (Maybe ItemRead)
          :<|> "shelf" :> Capture "id"    ShelfID    :> Get    '[JSON] (Maybe DepotRead)
          :<|> "shelf" :> Capture "label" ShelfLabel :> Get    '[JSON] [ItemRead]
          :<|>            ReqBody '[JSON] ItemWrite  :> Post   '[JSON] Int64
          :<|>            Capture "id"    ItemID     :> Delete '[JSON] Int64

itemAPI :: Proxy ItemAPI
itemAPI = Proxy

itemServer :: ServerT ItemAPI AppM
itemServer = getItems
        :<|> getItemById
        :<|> getItemsByShelfId
        :<|> getItemsByShelfLabel
        :<|> postItem
        :<|> deleteItem

getItems :: AppM [ItemRead]
getItems = do
  con <- ask
  liftIO $ runQuery con itemsQuery

getItemById :: ItemID -> AppM (Maybe ItemRead)
getItemById idToMatch = do
  con <- ask
  liftIO $ listToMaybe <$> runQuery con (itemByIdQuery idToMatch)

getItemsByShelfId :: ShelfID -> AppM (Maybe DepotRead)
getItemsByShelfId idToMatch = do
  con <- ask
  liftIO $ listToMaybe <$> runQuery con (itemsByShelfIdQuery idToMatch)

getItemsByShelfLabel :: ShelfLabel -> AppM [ItemRead]
getItemsByShelfLabel text = do
  con <- ask
  liftIO $ runQuery con (itemsByShelfLabelQuery text)

postItem :: ItemWrite -> AppM Int64
postItem item = do
  con <- ask
  liftIO $ runInsert con itemTable $ itemToPG item

deleteItem :: ItemID -> AppM Int64
deleteItem idToMatch = do
  con <- ask
  res <- liftIO $ runDelete con itemTable match
  case res of
    1 -> pure idToMatch
    _ -> throwError err404

  where
    match i = itId i .=== pgInt8 idToMatch

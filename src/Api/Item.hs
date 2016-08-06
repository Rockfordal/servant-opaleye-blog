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
import Models.Item
import Queries.Item

type ItemAPI = Get '[JSON] [ItemRead]
              -- :<|> Capture "id" ItemID      :> Get  '[JSON] (Maybe ItemRead)
              -- :<|> Capture "size" Size      :> Get  '[JSON] [ItemRead]
              :<|> ReqBody '[JSON] ItemWrite :> Post '[JSON] Int64

itemAPI :: Proxy ItemAPI
itemAPI = Proxy

itemServer :: ServerT ItemAPI AppM
itemServer = getItems
            -- :<|> getItemById
            -- :<|> getItemBySize
            :<|> itemPost

getItems :: AppM [ItemRead]
getItems = do
  con <- ask
  liftIO $ runQuery con itemQuery

-- getItemById :: ItemID -> AppM (Maybe ItemRead)
-- getItemById id = do
--   con <- ask
--   liftIO $ listToMaybe <$> runQuery con (itemByIdQuery id)

-- getItemsBySize :: Size -> AppM [ItemRead]
-- getItemsBySize size = do
--   con <- ask
--   liftIO $ runQuery con (itemsByEmailQuery email)

itemPost :: ItemWrite -> AppM Int64
itemPost item = do
  con <- ask
  liftIO $ runInsert con itemTable $ itemToPG item

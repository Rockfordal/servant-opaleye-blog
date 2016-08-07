{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Shelf where

import Servant
import Opaleye
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (listToMaybe)
import Data.Int (Int64)

import App
import Models.Shelf
import Queries.Shelf
import Queries.Depot

type ShelfAPI =                                         Get  '[JSON] [ShelfRead]
           :<|>           Capture "id"    ShelfID    :> Get  '[JSON] (Maybe ShelfRead)
           :<|>           Capture "label" ShelfLabel :> Get  '[JSON] [ShelfRead]
           :<|> "size" :> Capture "size"  ShelfSize  :> Get  '[JSON] [ShelfRead]
           :<|> "item" :> Capture "id"    ShelfID    :> Get  '[JSON] [ShelfRead]
           :<|>           ReqBody '[JSON] ShelfWrite :> Post '[JSON] Int64

shelfAPI :: Proxy ShelfAPI
shelfAPI = Proxy

shelfServer :: ServerT ShelfAPI AppM
shelfServer = getShelfs
         :<|> getShelfById
         :<|> getShelfsByLabel
         :<|> getShelfsBySize
         :<|> getShelfsByItemId
         :<|> shelfPost

getShelfs :: AppM [ShelfRead]
getShelfs = do
  con <- ask
  liftIO $ runQuery con shelfsQuery

getShelfById :: ShelfID -> AppM (Maybe ShelfRead)
getShelfById id = do
  con <- ask
  liftIO $ listToMaybe <$> runQuery con (shelfByIdQuery id)

getShelfsByLabel :: ShelfLabel -> AppM [ShelfRead]
getShelfsByLabel name = do
  con <- ask
  liftIO $ runQuery con (shelfsByLabelQuery name)

getShelfsBySize :: ShelfSize -> AppM [ShelfRead]
getShelfsBySize size = do
  con <- ask
  liftIO $ runQuery con (shelfsBySizeQuery size)

getShelfsByItemId :: ItemID -> AppM [ShelfRead]
getShelfsByItemId itemid = do
  con <- ask
  liftIO $ runQuery con (shelfsByItemIdQuery itemid)

shelfPost :: ShelfWrite -> AppM Int64
shelfPost shelf = do
  con <- ask
  liftIO $ runInsert con shelfTable $ shelfToPG shelf

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

type ShelfAPI = Get '[JSON] [ShelfRead]
              -- :<|> Capture "id" ShelfID       :> Get  '[JSON] (Maybe ShelfRead)
              -- :<|> Capture "email" Email      :> Get  '[JSON] [ShelfRead]
              -- :<|> ReqBody '[JSON] ShelfWrite :> Post '[JSON] Int64

shelfAPI :: Proxy ShelfAPI
shelfAPI = Proxy

shelfServer :: ServerT ShelfAPI AppM
shelfServer = getShelfs
            -- :<|> getShelfById
            -- :<|> getShelfByEmail
            -- :<|> shelfPost

getShelfs :: AppM [ShelfRead]
getShelfs = do
  con <- ask
  liftIO $ runQuery con shelfQuery

-- getShelfById :: ShelfID -> AppM (Maybe ShelfRead)
-- getShelfById id = do
--   con <- ask
--   liftIO $ listToMaybe <$> runQuery con (shelfByIdQuery id)

-- getShelfsByEmail :: Email -> AppM [ShelfRead]
-- getShelfsByEmail email = do
--   con <- ask
--   liftIO $ runQuery con (shelfsByEmailQuery email)

-- shelfPost :: ShelfWrite -> AppM Int64
-- shelfPost shelf = do
--   con <- ask
--   liftIO $ runInsert con shelfTable $ shelfToPG shelf

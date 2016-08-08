{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Product where

import Servant
import Opaleye
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (listToMaybe)
import Data.Int (Int64)

import App
import Models.Product
import Queries.Product

type ProductAPI =                                         Get  '[JSON] [ProductRead]
           :<|>           Capture "id"    ProductID    :> Get  '[JSON] (Maybe ProductRead)
           :<|>           Capture "name"  ProductName  :> Get  '[JSON] [ProductRead]
           :<|>           ReqBody '[JSON] ProductWrite :> Post '[JSON] Int64

productAPI :: Proxy ProductAPI
productAPI = Proxy

productServer :: ServerT ProductAPI AppM
productServer = getProducts
           :<|> getProductById
           :<|> getProductsByName
           :<|> productPost

getProducts :: AppM [ProductRead]
getProducts = do
  con <- ask
  liftIO $ runQuery con productsQuery

getProductById :: ProductID -> AppM (Maybe ProductRead)
getProductById id = do
  con <- ask
  liftIO $ listToMaybe <$> runQuery con (productByIdQuery id)

getProductsByName :: ProductName -> AppM [ProductRead]
getProductsByName name = do
  con <- ask
  liftIO $ runQuery con (productsByNameQuery name)

productPost :: ProductWrite -> AppM Int64
productPost product = do
  con <- ask
  liftIO $ runInsert con productTable $ productToPG product

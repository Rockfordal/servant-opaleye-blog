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

type ProductAPI =                                         Get    '[JSON] [ProductRead]
           :<|>           Capture "id"    ProductID    :> Get    '[JSON] (Maybe ProductRead)
           :<|>           Capture "name"  ProductName  :> Get    '[JSON] [ProductRead]
           :<|>           ReqBody '[JSON] ProductWrite :> Post   '[JSON] Int64
           :<|>           Capture "id"    ProductID    :> Delete '[JSON] Int64

productAPI :: Proxy ProductAPI
productAPI = Proxy

productServer :: ServerT ProductAPI AppM
productServer = getProducts
           :<|> getProductById
           :<|> getProductsByName
           :<|> postProduct
           :<|> deleteProduct

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

postProduct :: ProductWrite -> AppM Int64
postProduct product = do
  con <- ask
  liftIO $ runInsert con productTable $ productToPG product

deleteProduct :: ProductID -> AppM Int64
deleteProduct idToMatch = do
  con <- ask
  liftIO $ runDelete con productTable match
  where
    match = (\p -> (prId p) .=== (pgInt8 idToMatch))

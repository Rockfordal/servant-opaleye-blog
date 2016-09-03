{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Lib (startApp) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import App (AppM, readerTToExcept, www)
import Auth (basicAuthServerContext)
import Ware
import Api.User
import Api.BlogPost
import Api.Shelf
import Api.Item
import Api.Depot
import Api.Product
import Api.Private
import Api.Home
import Data.Text (Text)

type MainAPI = API :<|> Raw

type API = "users"    :> UserAPI
      :<|> "posts"    :> BlogPostAPI
      :<|> "shelfs"   :> ShelfAPI
      :<|> "items"    :> ItemAPI
      :<|> "depots"   :> DepotAPI
      :<|> "products" :> ProductAPI
      :<|> "private"  :> PrivateAPI
      :<|> "home"     :> HomeAPI
      -- :<|> IndexAPI


server :: ServerT API AppM
server = userServer
    :<|> blogPostServer
    :<|> shelfServer
    :<|> itemServer
    :<|> depotServer
    :<|> productServer
    :<|> privateServer
    :<|> homeServer
    -- :<|> fileServer
    -- :<|> indexServer

myServer :: Server MainAPI
-- myServer = enter readerTToExcept server
myServer = enter readerTToExcept server :<|> serveDirectory www

api :: Proxy API
api = Proxy

mainApi :: Proxy MainAPI
mainApi = Proxy

app :: Application
-- app = serveWithContext api basicAuthServerContext myServer
app = serveWithContext mainApi basicAuthServerContext myServer

startApp :: IO ()
startApp = run 3000 $ myCors app


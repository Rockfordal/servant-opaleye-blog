{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Lib (startApp) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import App (AppM, readerTToExcept)
import Auth (basicAuthServerContext)
import Ware
import Api.User
import Api.BlogPost
import Api.Shelf
import Api.Item
import Api.Depot
import Api.Product
import Api.Private
import Data.Text (Text)


type API = "users"    :> UserAPI
      :<|> "posts"    :> BlogPostAPI
      :<|> "shelfs"   :> ShelfAPI
      :<|> "items"    :> ItemAPI
      :<|> "depots"   :> DepotAPI
      :<|> "products" :> ProductAPI
      :<|> "private"  :> PrivateAPI

server :: ServerT API AppM
server = userServer
    :<|> blogPostServer
    :<|> shelfServer
    :<|> itemServer
    :<|> depotServer
    :<|> productServer
    :<|> privateServer

myServer :: Server API
myServer = enter readerTToExcept server

api :: Proxy API
api = Proxy

app :: Application
app = serveWithContext api basicAuthServerContext myServer

startApp :: IO ()
startApp = run 3000 $ myCors app

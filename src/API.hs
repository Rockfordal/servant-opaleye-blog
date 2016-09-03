{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module API where

import Servant (ServerT)
import Servant.API
import App (AppM)
import Api.User
import Api.BlogPost
import Api.Shelf
import Api.Item
import Api.Depot
import Api.Product
import Api.Private
import Api.Home


type API = "shelfs" :> ShelfAPI
      :<|> "posts"    :> BlogPostAPI
      :<|> "items"    :> ItemAPI
      :<|> "depots"   :> DepotAPI
      :<|> "products" :> ProductAPI
      :<|> "home"     :> HomeAPI
      :<|> "users"    :> UserAPI
      -- :<|> "private"  :> PrivateAPI


server :: ServerT API AppM
server = shelfServer
    :<|> blogPostServer
    :<|> itemServer
    :<|> depotServer
    :<|> productServer
    :<|> homeServer
    :<|> userServer
    -- :<|> privateServer

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
      -- :<|> "posts"    :> BlogPostAPI
      :<|> "items"    :> ItemAPI
      :<|> "depots"   :> DepotAPI
      -- :<|> "products" :> ProductAPI
      -- :<|> "private"  :> PrivateAPI
      -- :<|> "home"     :> HomeAPI
      -- :<|> "users"    :> UserAPI


server :: ServerT API AppM
server = shelfServer
--     :<|> blogPostServer
    :<|> itemServer
    :<|> depotServer
--     :<|> productServer
--     :<|> privateServer
--     :<|> homeServer
--     :<|> userServer

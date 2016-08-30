{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO)
import qualified Database.PostgreSQL.Simple as PGS

import App (AppM)
import Ware
import Api.User
import Api.BlogPost
import Api.Shelf
import Api.Item
import Api.Depot
import Api.Product
import Api.Private
import Data.Text (Text)
--import Models.User


type API = "users"    :> UserAPI
      :<|> "posts"    :> BlogPostAPI
      :<|> "shelfs"   :> ShelfAPI
      :<|> "items"    :> ItemAPI
      :<|> "depots"   :> DepotAPI
      :<|> "products" :> ProductAPI
      :<|> "private"  :> PrivateAPI
      -- :<|> "private" :> AuthProtect "cookie-auth" :> PrivateAPI


startApp :: IO ()
startApp = run 3000 $ myCors app

readerTToExcept :: AppM :~> Handler
readerTToExcept = Nat (\r -> do
  con <- liftIO $ PGS.connect PGS.defaultConnectInfo
                              { PGS.connectUser     = "blogtutorial"
                              , PGS.connectPassword = "blogtutorial"
                              , PGS.connectDatabase = "blogtutorial"
                              }
  runReaderT r con)

api :: Proxy API
api = Proxy

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

authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) = return $
        if username == "servant" && password == "server"
          then Authorized (User "servant")
          else Unauthorized
  in BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

app :: Application
-- app = serve api myServer
-- app = serveWithContext api EmptyContext myServer
app = serveWithContext api basicAuthServerContext myServer

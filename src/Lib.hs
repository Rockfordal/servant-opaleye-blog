{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Database.PostgreSQL.Simple as PGS

import App (AppM)
import Api.User
import Api.BlogPost
import Api.Shelf
import Api.Item
import Api.Depot

type API = "users"  :> UserAPI
      :<|> "posts"  :> BlogPostAPI
      :<|> "shelfs" :> ShelfAPI
      :<|> "items"  :> ItemAPI
      :<|> "depot"  :> DepotAPI


startApp :: IO ()
startApp = run 3000 app

readerTToExcept :: AppM :~> Handler
readerTToExcept = Nat (\r -> do con <- liftIO $ PGS.connect PGS.defaultConnectInfo
                                                          { PGS.connectUser     = "blogtutorial"
                                                          , PGS.connectPassword = "blogtutorial"
                                                          , PGS.connectDatabase = "blogtutorial"
                                                          }
                                runReaderT r con)

app :: Application
app = serve api $ enter readerTToExcept server

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = userServer
    :<|> blogPostServer
    :<|> shelfServer
    :<|> itemServer
    :<|> depotServer

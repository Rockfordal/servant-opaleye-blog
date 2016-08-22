{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
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
import Api.Auth
-- import Api.User
-- import Api.BlogPost
-- import Api.Shelf
-- import Api.Item
-- import Api.Depot
-- import Api.Product


-- type API =
      --      "users"    :> UserAPI
      -- :<|> "posts"    :> BlogPostAPI
      -- :<|> "shelfs"   :> ShelfAPI
      -- :<|> "items"    :> ItemAPI
      -- :<|> "depots"   :> DepotAPI
      -- :<|> "products" :> ProductAPI


startApp :: IO ()
startApp = run 3000 $ myCors app

-- readerTToExcept :: AppM :~> Handler
-- readerTToExcept = Nat (\r -> do
--   con <- liftIO $ PGS.connect PGS.defaultConnectInfo
--                               { PGS.connectUser     = "blogtutorial"
--                               , PGS.connectPassword = "blogtutorial"
--                               , PGS.connectDatabase = "blogtutorial"
--                               }
--   runReaderT r con)

-- app :: Application
-- app = serve api $ enter readerTToExcept server


app :: Application
-- app = enter readerTToExcept (serveWithContext basicAuthApi
app = (serveWithContext basicAuthApi
                        basicAuthServerContext
                        basicAuthServer)

api :: Proxy BasicAPI -- API
api = Proxy

-- server :: ServerT API AppM
-- server =
    --      userServer
    -- :<|> blogPostServer
    -- :<|> shelfServer
    -- :<|> itemServer
    -- :<|> depotServer
    -- :<|> productServer

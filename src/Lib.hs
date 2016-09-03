{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Lib (startApp) where

import Network.Wai
import Network.Wai.Handler.Warp
-- import Data.Swagger hiding (Header, Http)
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import App (AppM, readerTToExcept, www)
import Auth (basicAuthServerContext)
import Ware
import Data.Text (Text)
import Docs
import API


type MainAPI = API
          :<|> SwaggerSchemaEndpoint
          :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"
          :<|> Raw

myServer :: Server MainAPI
myServer = enter readerTToExcept server
      :<|> return swaggerDoc
      :<|> swaggerSchemaUIServer swaggerDoc
      :<|> serveDirectory www

api :: Proxy API
api = Proxy

mainApi :: Proxy MainAPI
mainApi = Proxy

app :: Application
app = serveWithContext mainApi basicAuthServerContext myServer

startApp :: IO ()
startApp = run 3000 $ myCors app


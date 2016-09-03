{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Docs where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Network.Wai.Handler.Warp
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Control.Lens hiding ((.=))
import Test.QuickCheck.Arbitrary
import qualified Data.Text.IO as TextIO
import Data.Text
import Data.Word
import Data.Swagger hiding (Header, Http)
import Data.Proxy
import Data.ByteString -- (ByteString)
import GHC.Generics
import Servant.API
import Servant.JS (writeJSForAPI, vanillaJS)
import Servant.Mock -- (mock)
import Servant.Docs hiding (API) -- (markdown, docs)
import qualified Lackey
import Servant.Swagger
import Servant.Swagger.UI
import Servant.Client
import Servant
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson (encode)
import Api.Shelf
import Models.Shelf
import Models.Depot
import Models.Item
import Models.Product
import Models.BlogPost
import Models.User
import App (ShelfID, DepotID, ItemID, ProductID, BlogPostID, Email)
import Data.DateTime (DateTime, fromSeconds)
import Data.Swagger.Schema
import API
-- import GHC.Generics.Generic (ByteString)

-- js :: IO ()
-- js = writeJSForAPI api vanillaJS (static </> "vanilla" </> "api.js")

-- rubyClient :: Text
-- rubyClient = Lackey.rubyForAPI api

-- writeRubyClient :: IO ()
-- writeRubyClient = TextIO.writeFile "rubyClient.rb" rubyClient

-- apiDocs :: String
-- apiDocs = markdown $ docs shelfapi

-- writeDocs :: IO ()
-- writeDocs = writeFile "mydocs.md" apiDocs

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy API)
    & info.title       .~ "Shelf API"
    & info.version     .~ "2016.7.7"
    & info.description ?~ "Hemlig API för hemlig projektutveckling"

-- mockServer :: IO ()
-- mockServer = run 3000 $ serve api $ mock api Proxy

-- genHello :: IO ()
-- genHello = BL8.putStr $ encode swaggerDoc

-- qhello :: Maybe String -> Manager -> BaseUrl -> ClientM HelloMessage
-- qhellouser :: Maybe Text -> Manager -> BaseUrl -> ClientM String
-- qgetusers :: Maybe Text -> Manager -> BaseUrl -> ClientM [User]
-- -- qgetusersnoauth :: Maybe String -> Maybe String -> Maybe Word16 -> Maybe Word16 -> Manager -> BaseUrl -> ClientM [User]
-- -- qhello :<|> qhellouser :<|> qgetusers :<|> qgetusersnoauth = client userapi
-- qhello :<|> qhellouser :<|> qgetusers = client userapi

-- queries :: Manager -> BaseUrl -> ExceptT ServantError IO (HelloMessage, String, [User], [User])
-- queries manager baseurl = do
--   let auth = Just "Basic dXNlcjpwYXNzd29yZA=="
--   msg    <- qhello     (Just "tjena") manager baseurl
--   secret <- qhellouser auth manager baseurl
--   users  <- qgetusers  auth manager baseurl
--   uusers <- qgetusers  auth manager baseurl
--   return (msg, secret, users, uusers)

-- haskell :: IO ()
-- haskell = do
--   let baseUrl = (BaseUrl Http "localhost" 3000 "/users") -- /users/hello
--   manager <- newManager defaultManagerSettings
--   res <- runExceptT $ queries manager baseUrl
--   case res of
--     Left err -> putStrLn $ "Error: " ++ show err
--     Right (msg, secret, users, uusers) -> do
--       print msg
--       print secret
--       print users
--       print uusers

type SwaggerSchemaEndpoint = "swagger.js" :> Get '[JSON] Swagger

-- type instance IsElem' e API = IsElem e API'
-- instance Arbitrary Swagger
-- instance Arbitrary (SwaggerUiHtml SwaggerSchemaEndpoint API)

instance ToSchema (Shelf' (Maybe ShelfID) String String Int (Maybe DateTime))
instance ToSchema (Shelf' ShelfID String String Int (Maybe DateTime))
instance ToSchema (Shelf' ShelfID String String Int DateTime)

instance ToSchema (Depot' DepotID ShelfID App.ItemID Int DateTime)
instance ToSchema (Depot' (Maybe DepotID) ShelfID ItemID Int (Maybe DateTime))

instance ToSchema (Item' (Maybe ItemID) String String (Maybe DateTime))
instance ToSchema (Item' ItemID String String DateTime)

instance ToSchema (Product' (Maybe ProductID) ShelfID String (Maybe DateTime))
instance ToSchema (Product' ProductID ShelfID String DateTime)

instance ToSchema (BlogPost' (Maybe BlogPostID) String String Email (Maybe DateTime))
instance ToSchema (BlogPost' BlogPostID String String Email DateTime)

instance ToSchema (User' Email String)
instance ToSchema (User' Email ByteString)

instance ToSchema ByteString
instance Generic ByteString

 -- • No instance for (Data.Swagger.Internal.Schema.GToSchema (Rep ByteString))

-- instance (Data.Swagger.Internal.Schema.GToSchema (Rep ByteString))
-- instance ToSchema (Rep ByteString)



--- Parametrar ---

-- instance ToParam (QueryParam "limit" Word16) where
--   toParam _ = DocQueryParam "limit" ["10", "20"] "Hejlimit" Normal -- List | Flag

-- instance ToParam (QueryParam "offset" Word16) where
--   toParam _ = DocQueryParam "offset" ["0", "10"] "Hejoffset" Normal -- List | Flag

-- instance ToParam (QueryParam "searchField" [Char]) where
--   toParam _ = DocQueryParam "searchField" ["title", "author"] "Hejsearchfield" Normal -- List | Flag

-- instance ToParam (QueryParam "searchStr" [Char]) where
--   toParam _ = DocQueryParam "searchStr" ["Kurt", "Anders"] "Hejsearchstr" Normal -- List | Flag

-- instance ToCapture (Capture "id" Int) where
--   toCapture _ = DocCapture "1" "Record number"

-- instance ToSample Char where
--   toSamples _ = [("apa", 'c')]

-- -- instance ToSample Int where
-- --   toSamples _ = noSamples

-- -- instance ToSample Book where
-- --   toSamples _ = noSamples

-- instance ToSample Char where
--   toSamples _ = [("apa", 'c')]


-- --- Mockserver ---
-- instance Arbitrary HelloMessage where
--   arbitrary = HelloMessage <$> arbitrary

-- instance Arbitrary User where
--   arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- -- instance Arbitrary Shelf where
-- --   arbitrary = Shelf <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- -- instance Arbitrary Item where
-- --   arbitrary = Item <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- -- instance Arbitrary Book where
-- --   arbitrary = Book <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- instance HasServer API
--   context where
--   type ServerT API m = ServerT API' m
--   route _ = route (Proxy :: Proxy API')

-- instance HasMock API context0

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Api.Home where

import Servant
import App
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
-- import Data.Text (Text)

-- newtype FileContent = FileContent
--   { content :: String }
--   deriving Generic

-- instance MimeRender PlainText FileContent where
--     mimeRender a = "hej"

type HomeAPI  = Get '[PlainText] String
-- type IndexAPI = Get '[PlainText] FileContent

homeServer :: AppM String
homeServer = pure "Tjena"

-- indexServer :: AppM FileContent
-- indexServer = do
--   filecontent <- liftIO (readFile "index.html")
--   return (FileContent filecontent)

-- fileServer :: Server StaticAPI
-- fileServer :: Server StaticAPI


-- fileServer :: AppM Raw
-- fileServer = do
--   serveDirectory "www"
  -- liftIO $ serveDirectory "www"

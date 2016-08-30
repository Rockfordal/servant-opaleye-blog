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

module Api.Private where

import Servant
import Opaleye
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
-- import Data.Maybe (listToMaybe)
-- import Data.Int (Int64)

import App
import Models.BlogPost
import Queries.BlogPost
import Data.Text (Text)


newtype User = User { userName :: Text }
  deriving (Eq, Show)

type PrivateAPI = BasicAuth "fire-realm" User :> Get '[JSON] [BlogPostRead]


privateAPI :: Proxy PrivateAPI
privateAPI = Proxy

privateServer :: User -> AppM [BlogPostRead]
privateServer _ = getPosts

getPosts :: AppM [BlogPostRead]
getPosts = do
  con <- ask
  liftIO $ runQuery con blogPostsQuery

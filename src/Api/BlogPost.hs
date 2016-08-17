{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api.BlogPost where

import Servant
import Opaleye
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (listToMaybe)
import Data.Int (Int64)

import App
import Models.BlogPost
import Queries.BlogPost


type BlogPostAPI = Get '[JSON] [BlogPostRead]
              :<|> Capture "id"    BlogPostID    :> Get    '[JSON] (Maybe BlogPostRead)
              :<|> Capture "email" Email         :> Get    '[JSON] [BlogPostRead]
              :<|> ReqBody '[JSON] BlogPostWrite :> Post   '[JSON] Int64
              :<|> Capture "id"    BlogPostID    :> Delete '[JSON] Int64

blogPostAPI :: Proxy BlogPostAPI
blogPostAPI = Proxy

blogPostServer :: ServerT BlogPostAPI AppM
blogPostServer = getPosts
            :<|> getPostById
            :<|> getPostsByEmail
            :<|> postPost
            :<|> deletePost

getPosts :: AppM [BlogPostRead]
getPosts = do
  con <- ask
  liftIO $ runQuery con blogPostsQuery

getPostById :: BlogPostID -> AppM (Maybe BlogPostRead)
getPostById idToMatch = do
  con <- ask
  liftIO $ listToMaybe <$> runQuery con (blogPostByIdQuery idToMatch)

getPostsByEmail :: Email -> AppM [BlogPostRead]
getPostsByEmail email = do
  con <- ask
  liftIO $ runQuery con (blogPostsByEmailQuery email)

postPost :: BlogPostWrite -> AppM Int64
postPost post = do
  con <- ask
  liftIO $ runInsert con blogPostTable $ blogPostToPG post

deletePost :: BlogPostID -> AppM Int64
deletePost idToMatch = do
  con <- ask
  liftIO $ runDelete con blogPostTable match
  where
    match p = bpId p .=== pgInt8 idToMatch

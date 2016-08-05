{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Book where

import Servant
import Opaleye
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (listToMaybe)
import Data.Int (Int64)

import App
import Models.Book
import Queries.Book

type BookAPI = Get '[JSON] [BookRead]
              -- :<|> Capture "id" BlogPostID       :> Get  '[JSON] (Maybe BlogPostRead)
              -- :<|> Capture "email" Email         :> Get  '[JSON] [BlogPostRead]
              -- :<|> ReqBody '[JSON] BlogPostWrite :> Post '[JSON] Int64

bookAPI :: Proxy BookAPI
blogPostAPI = Proxy

bookServer :: ServerT BlogPostAPI AppM
bookServer = getBooks
            -- :<|> getPostById
            -- :<|> getPostsByEmail
            -- :<|> postPost

getBook :: AppM [BookRead]
getBook = do
  con <- ask
  liftIO $ runQuery con bookQuery

getBookById :: BookID -> AppM (Maybe BookRead)
getBookById id = do
  con <- ask
  liftIO $ listToMaybe <$> runQuery con (bookByIdQuery id)

getBooksByEmail :: Email -> AppM [BookRead]
getBooksByEmail email = do
  con <- ask
  liftIO $ runQuery con (blogPostsByEmailQuery email)

bookPost :: BookWrite -> AppM Int64
bookPost book = do
  con <- ask
  liftIO $ runInsert con bookTable $ bookToPG book

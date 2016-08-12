{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api.User where

import Servant
import Opaleye
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (listToMaybe)
import Data.Int (Int64)

import App
import Models.User
import Queries.User

type UserAPI = Get '[JSON] [UserRead]
          :<|> Capture "email" Email :> Get '[JSON] (Maybe UserRead)
          :<|> "verify" :> ReqBody '[JSON] UserWrite :> Post '[JSON] Bool
          :<|> ReqBody '[JSON] UserWrite :> Post '[JSON] Int64
          -- :<|> Capture "id" UserID    :> Delete '[JSON] Int64

userAPI :: Proxy UserAPI
userAPI = Proxy

userServer :: ServerT UserAPI AppM
userServer = getUsers
        :<|> getUserByEmail
        :<|> verifyUser
        :<|> postUser
        -- :<|> deleteUser

getUsers :: AppM [UserRead]
getUsers = do
  con <- ask
  liftIO $ runQuery con usersQuery

getUserByEmail :: Email -> AppM (Maybe UserRead)
getUserByEmail email = do
  con <- ask
  liftIO $ listToMaybe <$> runQuery con (userByEmailQuery email)

verifyUser :: UserWrite -> AppM Bool
verifyUser user = do
  dbUser <- getUserByEmail (userEmail user)
  return $ compareUsers dbUser user

postUser :: UserWrite -> AppM Int64
postUser user = do
  con <- ask
  newUser <- liftIO $ userToPG user
  liftIO $ runInsert con userTable newUser

-- deleteUser :: UserID -> AppM Int64
-- deleteUser idToMatch = do
--   con <- ask
--   liftIO $ runDelete con userTable match
--   where
--     match = (\d -> (uId d) .=== (pgInt8 idToMatch))

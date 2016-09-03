{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings     #-}
module Auth where
import Servant
import Api.Private
import Data.Text (Text)


authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) = return $
        if username == "servant" && password == "server"
          then Authorized (User "servant")
          else Unauthorized
  in BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

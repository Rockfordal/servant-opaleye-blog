{-# LANGUAGE TypeOperators         #-}
module App where

import Data.Int (Int64)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Servant (ServantErr)
import Database.PostgreSQL.Simple (Connection)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO)
import qualified Database.PostgreSQL.Simple as PGS
import Servant

type AppM = ReaderT Connection (ExceptT ServantErr IO)

type BlogPostID = Int64
type ShelfID    = Int64
type ItemID     = Int64
type DepotID    = Int64
type ProductID  = Int64
-- type UserID = Int64

type ShelfSize   = Int
type Email       = String
type ShelfLabel  = String
type ProductName = String


readerTToExcept :: AppM :~> Handler
readerTToExcept = Nat (\r -> do
  con <- liftIO $ PGS.connect PGS.defaultConnectInfo
                              { PGS.connectUser     = "blogtutorial"
                              , PGS.connectPassword = "blogtutorial"
                              , PGS.connectDatabase = "blogtutorial" }
  runReaderT r con)

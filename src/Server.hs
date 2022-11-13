module Server
  ( server
  , Env(..)
  , module X
  ) where

import Servant
import Server.GetMessage qualified as GetMessage
import Server.ListTag    qualified as ListTag
import Server.Save       qualified as Save
import Server.ToggleLog  qualified as ToggleLog

import Api as X

-- | Service environment by methods
data Env = Env
  { save        :: Save.Env
  , getMessage  :: GetMessage.Env
  , listTag     :: ListTag.Env
  , toggleLogs  :: ToggleLog.Env
  }

-- | Servant server for the app
server :: Env -> ServerT Api IO
server env =
       Save.handle env.save
  :<|> GetMessage.handle env.getMessage
  :<|> ListTag.handle env.listTag
  :<|> ToggleLog.handle env.toggleLogs

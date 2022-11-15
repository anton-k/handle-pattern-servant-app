-- | Get by tag handler
module Server.ListTag
  ( Env(..)
  , Db(..)
  , handle
  , dbLog
  ) where

import DI.Log
import Types

data Env = Env
  { db  :: Db
  , log :: Log
  }

data Db = Db
  { listTag :: Tag -> IO [Message]
  }

-- | Adapts DB interface so that every call to it get's logged
-- under storage context
dbLog :: Log -> Db -> Db
dbLog logger (Db listTag) =
  Db $ logFun logger "storage" "Db.listTag" Just listTag

-----------------------------------------
-- Handler

handle :: Env -> Tag -> IO [Message]
handle (Env Db{..} Log{..}) tag = do
  logInfo $ "list tag call: " <> display tag
  listTag tag

-- | Save message handler
module Server.Save
  ( Env(..)
  , Db(..)
  , handle
  ) where

import DI.Log
import DI.Time

import Api
import Types

data Db = Db
  { saveMessage :: Message -> IO MessageId
  }

data Env = Env
  { log   :: Log
  , db    :: Db
  , time  :: Time
  }

-----------------------------------------
-- Handler

handle :: Env -> SaveRequest -> IO SaveResponse
handle (Env Log{..} Db{..} Time{..}) req = do
  logInfo $ "save call: " <> display req
  time <- now
  let msg = Message req.message req.tags time
  logInfo $ "create message: " <> display msg
  SaveResponse <$> saveMessage msg

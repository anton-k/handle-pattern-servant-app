-- | Toggle logs handler
module Server.ToggleLog
  ( Env(..)
  , handle
  ) where

import DI.Log
import DI.Setup

data Env = Env
  { log   :: Log
  , setup :: Setup
  }

-----------------------------------------
-- Handler

handle :: Env -> IO ()
handle (Env Log{..} Setup{..}) = do
  logInfo "toggle log call"
  toggleLogs

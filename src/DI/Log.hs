-- | Logger interface and utilities
module DI.Log
  ( Log(..)
  , mapLog
  , addLogContext
  , display
  ) where

import Data.Text qualified as Text
import Types

data Log = Log
  { logInfo  :: Text -> IO ()
  , logDebug :: Text -> IO ()
  , logError :: Text -> IO ()
  }

addLogContext :: Text -> Log -> Log
addLogContext contextMesage =
  mapLog (mappend (contextMesage <> ": "))

mapLog :: (Text -> Text) -> Log -> Log
mapLog go logger = Log
  { logInfo = logger.logInfo . go
  , logDebug = logger.logDebug . go
  , logError = logger.logError . go
  }

display :: Show a => a -> Text
display = Text.pack . show

-- | Logger interface and utilities
module DI.Log
  ( Log(..)
  , mapLog
  , addLogContext
  , display
  , logFun
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

-- | Wrap function call with logging
-- with possible failure report
--
-- if getResult argument returns Nothing we treat it as failure
logFun :: (Show a, Show c)
  => Log
  -> Text
  -> Text
  -> (b -> Maybe c)
  -> (a -> IO b) -> a -> IO b
logFun logger context funName getResult fun arg = do
  logInfo $ echoInput "Call"
  res <- fun arg
  case getResult res of
    Just out -> logInfo $ Text.unwords ["Result of", funName, "call:", display out]
    Nothing  -> logError $ echoInput "Failed to get result for"
  pure res
  where
    Log{..} = addLogContext context logger
    echoInput prefix = Text.unwords [prefix, funName, "with", display arg]

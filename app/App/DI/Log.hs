-- | Init logs
module App.DI.Log
  ( initLog
  ) where

import Control.Monad
import Data.Text.IO qualified as Text
import DI.Log
import App.State (VerboseVar, isVerbose)

initLog :: VerboseVar -> IO Log
initLog config = do
  pure $ Log
    { logInfo = logConsole "INFO"
    , logError = logConsole "ERROR"
    , logDebug = logConsole "DEBUG"
    }
  where
    logConsole tag txt = do
      ok <- isVerbose config
      when ok $ Text.putStrLn $ mconcat ["[", tag, "]: ", txt ]


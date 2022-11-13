-- | init time service
module App.DI.Time
  ( initTime
  ) where

import Data.Time
import DI.Time
import Types

initTime :: Url -> IO Time
initTime _url = do
  pure $ Time
    { now = getCurrentTime
    }

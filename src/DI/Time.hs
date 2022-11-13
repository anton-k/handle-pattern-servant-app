-- | Time-service interface, it's here as an example of
-- external service that talks to the outside world.
-- Let's imagine that we ask current time over http-client
module DI.Time
  ( Time(..)
  ) where

import Types

-- | Get current time
data Time = Time
  { now :: IO UTCTime
  }

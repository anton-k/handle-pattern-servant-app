-- | Setup and update configs and initialization params
module DI.Setup
  ( Setup(..)
  ) where

-- | Setup app configs
data Setup = Setup
  { toggleLogs :: IO ()  -- ^ toggle log verbosity
  }

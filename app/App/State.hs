-- | Application mutable state
module App.State
  ( VerboseVar
  , newVerboseVar
  , isVerbose
  , toggleVerbose
  ) where

import Control.Concurrent.STM

newtype VerboseVar = VerboseVar (TVar Bool)

newVerboseVar :: IO VerboseVar
newVerboseVar = VerboseVar <$> newTVarIO True

isVerbose :: VerboseVar -> IO Bool
isVerbose (VerboseVar var) = readTVarIO var

toggleVerbose :: VerboseVar -> IO ()
toggleVerbose (VerboseVar var) = atomically $ modifyTVar' var not

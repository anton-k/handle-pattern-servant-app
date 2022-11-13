-- | App setup interface
module App.DI.Setup
  ( initSetup
  ) where

import DI.Setup
import App.State

initSetup :: VerboseVar -> Setup
initSetup var =
  Setup
    { toggleLogs = toggleVerbose var
    }

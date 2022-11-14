-- | Get by tag handler
module Server.ListTag
  ( Env(..)
  , Db(..)
  , Foo(..)
  , handle
  ) where

import DI.Log
import Types

data Env = Env
  { db  :: Db
  , log :: Log
  , foo :: Foo
  }

data Foo = Foo
  { validTag :: Tag -> IO Bool
  }

data Db = Db
  { listTag :: Tag -> IO [Message]
  }

-----------------------------------------
-- Handler

handle :: Env -> Tag -> IO [Message]
handle (Env Db{..} Log{..} _foo) tag = do
  logInfo $ "list tag call: " <> display tag
  listTag tag

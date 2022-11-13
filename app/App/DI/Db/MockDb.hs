-- | Mock in memory DB
module App.DI.Db.MockDb
  ( MockDb
  , newMockDb
  , insertDb
  , lookupDb
  , lookupByTag
  ) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Types

newtype MockDb = MockDb (TVar (Map MessageId Message))

newMockDb :: IO MockDb
newMockDb = MockDb <$> newTVarIO mempty

newId :: MockDb -> STM MessageId
newId (MockDb tvar) = MessageId . Map.size <$> readTVar tvar

insertDb :: MockDb -> Message -> IO MessageId
insertDb db@(MockDb tvar) msg = atomically $ do
  msgId <- newId db
  modifyTVar' tvar (Map.insert msgId msg)
  pure msgId

lookupDb :: MockDb -> MessageId -> IO (Maybe Message)
lookupDb (MockDb tvar) msgId = Map.lookup msgId <$> readTVarIO tvar

lookupByTag :: MockDb -> Tag -> IO [Message]
lookupByTag (MockDb tvar) tag =
  filter (\msg -> elem tag msg.tags) . Map.elems <$> readTVarIO tvar

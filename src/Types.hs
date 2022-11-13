-- | Domain types
module Types
  ( Url
  , MessageId(..)
  , Tag(..)
  , Message(..)
  , module X
  ) where

import Data.Time as X (UTCTime)
import Data.Text as X (Text)
import GHC.Records as X (HasField(..))
import Deriving.Aeson
import Deriving.Aeson.Stock
import Servant.API (FromHttpApiData)
import App as X

type Url = String

newtype MessageId = MessageId { unMessageId :: Int }
  deriving newtype (ToJSON, FromJSON, Show, Eq, Ord, FromHttpApiData)

newtype Tag = Tag { unTag :: Text }
  deriving newtype (ToJSON, FromJSON, Show, Eq, FromHttpApiData)

data Message = Message
  { content :: Text
  , tags    :: [Tag]
  , time    :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Vanilla Message


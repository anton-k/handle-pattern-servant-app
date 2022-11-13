-- | API for the service.
-- For simplicity it is in single module, but will be split on parts
-- by methods in real project.
module Api
  ( Api
  , SaveRequest(..)
  , SaveResponse(..)
  ) where

import Deriving.Aeson
import Deriving.Aeson.Stock
import Servant.API
import Types

type Api = "api" :> "v1" :>
  (    SaveMessage
  :<|> GetById
  :<|> GetByTag
  :<|> ToggleLog
  )

type SaveMessage =
  "save" :> ReqBody '[JSON] SaveRequest :> Post '[JSON] SaveResponse

--------------------------------------------------------------------------
-- save

data SaveRequest = SaveRequest
  { message :: Text
  , tags    :: [Tag]
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Vanilla SaveRequest

newtype SaveResponse = SaveResponse MessageId
  deriving newtype (ToJSON, FromJSON, Show, Eq)

--------------------------------------------------------------------------
-- get by id

type GetById =
  "get" :> "message" :> Capture "id" MessageId :> Get '[JSON] Message

--------------------------------------------------------------------------
-- get by tag

type GetByTag =
  "list" :> "tag" :> Capture "tag" Tag :> Get '[JSON] [Message]

--------------------------------------------------------------------------
-- toggle log

type ToggleLog =
  "toggle-logs" :> Post '[JSON] ()


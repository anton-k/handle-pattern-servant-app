-- | Code to throw custom errors
module Error
  ( ApiError (..)
  , throwApi
  ) where

import Data.Text (Text)
import Control.Exception (throwIO)
import Servant.Server as Servant
import Data.ByteString.Lazy   qualified as BL
import Data.Text.Encoding     qualified as Text

-- | we use plain Text but in real app it's going to be
-- a Sum type which describes possible exceptions of our app
newtype ApiError = ApiError Text

throwApi :: ApiError -> IO a
throwApi = throwIO . toServantError
  where
    toServantError (ApiError err) = err400 { errBody = BL.fromStrict $ Text.encodeUtf8 err }


-- | An example of how to read server configs from YAML
-- and override them with CLI options
module Config
  ( Config(..)
  , readConfig
  ) where

import GHC.Generics
import Types
import Data.Maybe (fromMaybe)
import Data.Yaml qualified as Yaml
import Deriving.Aeson.Stock
import Options.Applicative

-- | Config for the server
data Config = Config
  { db   :: Url  -- ^ path to DB
  , time :: Url  -- ^ path to timer service
  , port :: Int  -- ^ server port
  }
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via Vanilla Config

defaultConfig :: Config
defaultConfig =
  Config
    { db   = "path/db"
    , time = "path/time"
    , port = 7070
    }

-----------------------------------------------------------------
-- parsing configs with CLI options

-- | CLI args
data Args = Args
  { config       :: Maybe FilePath
  , portOverride :: Maybe Int   -- ^ an example of option that can override config
  }

overrideArgs :: Args -> Config -> Config
overrideArgs args cfg =
  cfg { port = override args.portOverride cfg.port }
  where
    override ma a = fromMaybe a ma

readConfig :: IO Config
readConfig = do
  args <- execParser opts
  overrideArgs args <$> case args.config of
    Just configFile -> either (const defaultConfig) id <$> Yaml.decodeFileEither configFile
    Nothing -> pure defaultConfig
  where

    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Message server (example of haskell web app)"
     <> header "handler-proto - an example for haskell web app" )
     where
      args = Args <$> optional configFile
                  <*> optional portOpt

      configFile = strOption
          (  long "config"
          <> short 'c'
          <> metavar "FILE"
          <> help "YAML-config file for the server"
          )

      portOpt = option auto
          ( long "port"
         <> help "server port"
         <> metavar "INT"
         )

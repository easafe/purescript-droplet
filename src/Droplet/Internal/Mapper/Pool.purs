module Droplet.Internal.Mapper.Pool where

import Prelude (bind, flip, pure, ($))

import Data.Either (hush)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.String.CodeUnits (singleton)
import Data.Traversable (foldMap)
import Effect (Effect)

-- | PostgreSQL connection pool.
foreign import data Pool :: Type

type Database = String

-- | Configuration which we actually pass to FFI.
type Configuration'
      = { user :: Nullable String
            , password :: Nullable String
            , host :: Nullable String
            , port :: Nullable Int
            , database :: String
            , max :: Nullable Int
            , idleTimeoutMillis :: Nullable Int
            }

-- | PostgreSQL connection pool configuration.
type Configuration
      = { database :: Database
            , host :: Maybe String
            , idleTimeoutMillis :: Maybe Int
            , max :: Maybe Int
            , password :: Maybe String
            , port :: Maybe Int
            , user :: Maybe String
            }

type PGConnectionURI
      = String

defaultConfiguration :: Database -> Configuration
defaultConfiguration database =
      { database
      , host: Nothing
      , idleTimeoutMillis: Nothing
      , max: Nothing
      , password: Nothing
      , port: Nothing
      , user: Nothing
      }

foreign import ffiNew ::
      Configuration' ->
      Effect Pool

-- | Create a new connection pool.
new :: Configuration -> Effect Pool
new cfg = ffiNew $ cfg'
      where
      cfg' =
            { user: toNullable cfg.user
            , password: toNullable cfg.password
            , host: toNullable cfg.host
            , port: toNullable cfg.port
            , database: cfg.database
            , max: toNullable cfg.max
            , idleTimeoutMillis: toNullable cfg.idleTimeoutMillis
            }

foreign import totalCount :: Pool -> Effect Int

foreign import idleCount :: Pool -> Effect Int

foreign import waitingCount :: Pool -> Effect Int


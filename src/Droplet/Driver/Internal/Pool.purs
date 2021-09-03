module Droplet.Driver.Internal.Pool (Configuration, Database, Pool, defaultConfiguration, newPool) where

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)

-- | PostgreSQL connection pool.
foreign import data Pool ∷ Type

type Database = String

-- | Configuration which we actually pass to FFI.
type Configuration' =
      { user ∷ Nullable String
      , password ∷ Nullable String
      , host ∷ Nullable String
      , port ∷ Nullable Int
      , database ∷ String
      , max ∷ Nullable Int
      , idleTimeoutMillis ∷ Nullable Int
      }

-- | PostgreSQL connection pool configuration.
type Configuration =
      { database ∷ Database
      , host ∷ Maybe String
      , idleTimeoutMillis ∷ Maybe Int
      , max ∷ Maybe Int
      , password ∷ Maybe String
      , port ∷ Maybe Int
      , user ∷ Maybe String
      }

foreign import newPool_ ∷ Configuration' → Effect Pool

defaultConfiguration ∷ Database → Configuration
defaultConfiguration database =
      { database
      , host: Nothing
      , idleTimeoutMillis: Nothing
      , max: Nothing
      , password: Nothing
      , port: Nothing
      , user: Nothing
      }

-- | Create a new connection pool.
newPool ∷ Configuration → Effect Pool
newPool cfg = newPool_
      { user: toNullable cfg.user
      , password: toNullable cfg.password
      , host: toNullable cfg.host
      , port: toNullable cfg.port
      , database: cfg.database
      , max: toNullable cfg.max
      , idleTimeoutMillis: toNullable cfg.idleTimeoutMillis
      }


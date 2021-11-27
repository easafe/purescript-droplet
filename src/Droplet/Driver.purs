-- | Functions for database access
module Droplet.Driver (module Exported) where

import Droplet.Language.Internal.Translate (Query) as Exported

import Droplet.Driver.Internal.Query (class FromResult, Connection, PGErrorDetail, PgError(..), connect, execute, query, single, toResult, withConnection, withTransaction) as Exported

import Droplet.Driver.Internal.Pool (Configuration, Database, Pool, defaultConfiguration, newPool) as Exported
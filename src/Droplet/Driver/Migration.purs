module Droplet.Driver.Migration (Migration, migrate) where

import Prelude

import Data.Array as DA
import Data.Either (Either(..))
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as DS
import Debug (spy)
import Droplet.Driver.Internal.Migration as DDIM
import Droplet.Driver.Internal.Pool (Pool)
import Droplet.Driver.Internal.Query (Connection)
import Droplet.Driver.Internal.Query as DDIQ
import Droplet.Language (class ToParameters)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Prim.RowList (class RowToList)

-- | Type for individual migrations
-- |
-- | - `up` performs a migration
-- | - `down` reverts a migration
-- | - `identifier` must be unique
type Migration =
      { up ∷ Connection → Aff Unit
      , down ∷ Connection → Aff Unit
      , identifier ∷ String
      }

-- | Runs migrations
-- |
-- | All migrations run in a single transaction. Migrations that have already been run are skipped
migrate ∷ Pool → Array Migration → Aff Unit
migrate pool migrations = do
      output <- DDIQ.withTransaction pool $ \connection -> do
            DDIM.createMigrationTable connection
            identifiers <- DDIM.fetchAlreadyRun connection
            DF.traverse_ (runMigration connection) $ skipAlreadyRun identifiers migrations
      case output of
            Left err → DDIM.throw err
            Right _ → pure unit

runMigration ∷ Connection → Migration → Aff Unit
runMigration connection migration = do
      migration.up connection
      DDIM.markAsRun connection migration

skipAlreadyRun ∷ Set String → Array Migration → Array Migration
skipAlreadyRun identifiers = DA.filter skip
      where skip migration = not $ DS.member migration.identifier identifiers

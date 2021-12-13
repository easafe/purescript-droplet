module Droplet.Driver.Migration (Migration, migrate) where

import Prelude

import Data.Array as DA
import Data.Either (Either(..))
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as DS
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
            createMigrationTable connection
            identifiers <- fetchAlreadyRun connection
            DF.traverse_ (runMigration connection) $ skipAlreadyRun identifiers migrations
      case output of
            Left err → throw err
            Right _ → pure unit

runMigration ∷ Connection → Migration → Aff Unit
runMigration connection migration = do
      migration.up connection
      markAsRun connection migration

fetchAlreadyRun ∷ Connection → Aff (Set String)
fetchAlreadyRun connection = do
      output ← DDIQ.unsafeQuery connection Nothing "SELECT identifier FROM migrations" {}
      case output of
            Right (rows :: Array { identifier :: String }) -> pure <<< DS.fromFoldable $ map _.identifier rows
            Left err → throw err

skipAlreadyRun ∷ Set String → Array Migration → Array Migration
skipAlreadyRun identifiers = DA.filter skip
      where skip migration = not $ DS.member migration.identifier identifiers

markAsRun ∷ Connection → Migration → Aff Unit
markAsRun connection migration =
      execute connection "INSERT INTO migrations (identifier) VALUES (@identifier)" {identifier: migration.identifier}

createMigrationTable ∷ Connection → Aff Unit
createMigrationTable connection = execute connection "CREATE TABLE IF NOT EXISTS migrations (identifier TEXT NOT NULL PRIMARY KEY, run_at TIMESTAMPTZ NOT NULL DEFAULT NOW())" {}

execute :: forall parameters list. RowToList parameters list => ToParameters parameters list => Connection -> String -> Record parameters -> Aff Unit
execute connection sql parameters = do
      output <- DDIQ.unsafeExecute connection Nothing sql parameters
      case output of
            Just err -> throw err
            Nothing -> pure unit


throw err = EA.throwError <<< EA.error $ show err
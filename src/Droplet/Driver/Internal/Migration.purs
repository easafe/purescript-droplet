module Droplet.Driver.Internal.Migration where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as DS
import Droplet.Driver.Internal.Query (Connection(..))
import Droplet.Driver.Internal.Query as DDIQ
import Droplet.Language (class ToParameters)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Prim.RowList (class RowToList)

markAsRun ∷ forall r. Connection → { identifier :: String | r } → Aff Unit
markAsRun connection migration =
      execute connection "INSERT INTO __droplet_migrations__ (identifier) VALUES (@identifier)" {identifier: migration.identifier}

createMigrationTable ∷ Connection → Aff Unit
createMigrationTable connection = execute connection "CREATE TABLE IF NOT EXISTS __droplet_migrations__ (identifier TEXT NOT NULL PRIMARY KEY, run_at TIMESTAMPTZ NOT NULL DEFAULT NOW())" {}

execute :: forall parameters list. RowToList parameters list => ToParameters parameters list => Connection -> String -> Record parameters -> Aff Unit
execute connection sql parameters = do
      output <- DDIQ.unsafeExecute connection Nothing sql parameters
      case output of
            Just err -> throw err
            Nothing -> pure unit

fetchAlreadyRun ∷ Connection → Aff (Set String)
fetchAlreadyRun connection = do
      output ← DDIQ.unsafeQuery connection Nothing "SELECT identifier FROM __droplet_migrations__" {}
      case output of
            Right (rows :: Array { identifier :: String }) -> pure <<< DS.fromFoldable $ map _.identifier rows
            Left err → throw err

throw err = EA.throwError <<< EA.error $ show err

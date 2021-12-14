module Test.Migration where

import Droplet.Language
import Prelude
import Test.Types

import Control.Monad.Error.Class as CMEC
import Control.Monad.Error.Class as EA
import Data.Maybe (Maybe(..))
import Droplet.Driver as DD
import Droplet.Driver.Internal.Migration as DDIM
import Droplet.Driver.Internal.Query as DDIQ
import Droplet.Driver.Migration as DDM
import Effect.Class (liftEffect)
import Effect.Exception as EE
import Test.Model (connectionInfo)
import Test.Model as TM
import Test.Spec (Spec)
import Test.Spec as TS

tests ∷ Spec Unit
tests = TS.describe "migration" do
      TS.it "runs all migrations" do
            pool ← liftEffect $ DD.newPool connectionInfo
            DDM.migrate pool
                  [ { up: \c → void <<< DD.execute c $ create # table migrated, down: const (pure unit), identifier: "aaa" }
                  , { up: \c → void $ DDIQ.execute c $ insert # into migrated id # values 3, down: const (pure unit), identifier: "bbb" }
                  ]
            let q = select id # from migrated
            TM.result q [ { id: 3 } ]
      TS.it "doesn't fail if table already exists" do
            pool ← liftEffect $ DD.newPool connectionInfo
            void $ DD.withTransaction pool $ \c → DDIM.createMigrationTable c
            DDM.migrate pool []
      TS.it "skip migrations already run" do
            pool ← liftEffect $ DD.newPool connectionInfo
            -- mark step as run
            void $ DD.withTransaction pool $ \c → do
                  DDIM.createMigrationTable c
                  DDIM.markAsRun c { identifier: "bbb" }
            DDM.migrate pool
                  [ { up: \c → void <<< DD.execute c $ create # table migrated, down: const (pure unit), identifier: "aaa" }
                  , { up: \c → void $ DDIQ.execute c $ insert # into migrated id # values 3, down: const (pure unit), identifier: "bbb" }
                  ]
            let q = select id # from migrated
            TM.result q []
      TS.it "all or nothing" do
            pool ← liftEffect $ DD.newPool connectionInfo
            void $ DD.withTransaction pool $ \c → DD.execute c $ create # table migrated
            void $ CMEC.try $ DDM.migrate pool
                  [ { up: \c → void $ DDIQ.execute c $ insert # into migrated id # values 3, down: const (pure unit), identifier: "bbb" }
                  , { up: const (EA.throwError $ EE.error "error"), down: const (pure unit), identifier: "ccc" }
                  ]
            let q = select id # from migrated
            TM.result q []
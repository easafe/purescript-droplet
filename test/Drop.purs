module Test.Drop where

import Droplet.Language
import Prelude
import Prim hiding (Constraint)
import Test.Types

import Data.Maybe (Maybe(..))
import Droplet.Driver as DD
import Droplet.Driver.Unsafe as DDU
import Droplet.Language.Internal.Translate as DLIQ
import Effect.Class (liftEffect)
import Test.Model (connectionInfo)
import Test.Model as TM
import Test.Spec (Spec)
import Test.Spec as TS
            
tests ∷ Spec Unit
tests =
      TS.describe "drop" do
            TS.it "table" do
                  pool ← liftEffect $ DD.newPool connectionInfo
                  void $ DD.withTransaction pool $ \c → DDU.unsafeExecute c Nothing "CREATE TABLE test (id INTEGER)" {}
                  let q = drop # table (Table :: Table "test" (id :: Int))
                  TM.notParameterized """DROP TABLE "test"""" $ DLIQ.buildQuery q
                  void $ TM.resultOnly q
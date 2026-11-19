module Test.Prepare where

import Droplet.Language
import Prelude
import Test.Types

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM
import Test.Spec (Spec)
import Test.Spec as TS
import Test.Spec.Assertions as TSA

tests ∷ Spec Unit
tests =
      TS.describe "prepare" do
            TS.it "select" do
                  let q = prepare (Plan "select_user_name") $ select name # from users # wher (id .=. 2)
                  TM.parameterized """SELECT "name" FROM "users" WHERE "id" = $1""" $ DLIQ.buildQuery q
                  TSA.shouldEqual (Just "select_user_name") (planName (spyQuery q).plan)
                  TM.result q [ { name: "mary" } ]
            TS.it "insert" do
                  let q = prepare (Plan "insert_user") $ insert # into users (name /\ surname) # values ("mary" /\ "m.") # returning id
                  TM.parameterized """INSERT INTO "users" ("name", "surname") VALUES ($1, $2) RETURNING "id"""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 3 } ]
      where
      planName = map (\(Plan p) → p)

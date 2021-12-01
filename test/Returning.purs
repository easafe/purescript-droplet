module Test.Returning where

import Droplet.Language
import Prelude
import Test.Types

import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM

import Test.Spec (Spec)
import Test.Spec as TS


tests ∷ Spec Unit
tests = do
      TS.describe "returning" do
            TS.it "default values" do
                  let q = insert # into doublePrimaryKey defaultValues # returning id
                  TM.notParameterized """INSERT INTO "double_primary_key" DEFAULT VALUES RETURNING "id"""" $ DLIQ.buildQuery q
                  TM.result' q [ { id: 1 } ]
            TS.it "single field" do
                  let q = insert # into users (name /\ surname /\ birthday /\ joined) # values ("mary" /\ "m." /\ TM.makeDate 2000 9 9 /\ TM.makeDate 2009 9 9) # returning id
                  TM.parameterized """INSERT INTO "users"("name", "surname", "birthday", "joined") VALUES ($1, $2, $3, $4) RETURNING "id"""" $ DLIQ.buildQuery q
                  TM.result' q [ { id: 1 } ]
            TS.it "tuple" do
                  let q = insert # into users (name /\ surname /\ birthday /\ joined) # values ("mary" /\ "m." /\ TM.makeDate 2000 9 9 /\ TM.makeDate 2009 9 9) # returning (id /\ name)
                  TM.parameterized """INSERT INTO "users"("name", "surname", "birthday", "joined") VALUES ($1, $2, $3, $4) RETURNING "id", "name"""" $ DLIQ.buildQuery q
                  TM.result' q [ { id: 1, name: "mary" } ]
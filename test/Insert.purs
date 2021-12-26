module Test.Insert where

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
      TS.describe "insert" do
            TS.describe "values" do
                  TS.it "all fields" do
                        let q = insert # into users (name /\ surname /\ birthday /\ joined) # values ("mary" /\ "m." /\ TM.makeDate 2000 9 9 /\ TM.makeDate 2009 9 9)
                        TM.parameterized """INSERT INTO "users"("name", "surname", "birthday", "joined") VALUES ($1, $2, $3, $4)""" $ DLIQ.buildQuery q
                        TM.result' q []
                  TS.it "some fields" do
                        let q = insert # into tags name # values "my tag"
                        TM.parameterized """INSERT INTO "tags"("name") VALUES ($1)""" $ DLIQ.buildQuery q
                        TM.result' q []
                  TS.describe "default values" do
                        TS.it "single" do
                              let q = insert # into users (name /\ surname /\ joined) # values ("josh" /\ "a" /\ Default)
                              TM.parameterized """INSERT INTO "users"("name", "surname", "joined") VALUES ($1, $2, DEFAULT)""" $ DLIQ.buildQuery q
                              TM.result' q []
                        TS.it "all" do
                              let q = insert # into doublePrimaryKey defaultValues
                              TM.notParameterized """INSERT INTO "double_primary_key" DEFAULT VALUES""" $ DLIQ.buildQuery q
                              TM.result' q []
                        -- need some different design for this
                        -- TS.it "many" do
                        --       let q = insert # into users (name /\ surname /\ joined) # values ["josh" /\ "a" /\ TM.makeDate 2000 3 4, "josh" /\ "a" /\ Default]
                        --       TM.parameterized """INSERT INTO "users"("name", "surname", "joined") VALUES ($1, $2, $3), ($4, $5, DEFAULT)""" $ DLIQ.buildQuery q
                        --       TM.result' q []
                  TS.describe "multiple" do
                        TS.it "all fields" do
                              let
                                    q = insert # into users (name /\ surname /\ birthday /\ joined) # values
                                          [ "mary" /\ "m." /\ TM.makeDate 2000 9 9 /\ TM.makeDate 2009 9 9
                                          , "john" /\ "j." /\ TM.makeDate 2000 9 9 /\ TM.makeDate 2009 9 9
                                          ]
                              TM.parameterized """INSERT INTO "users"("name", "surname", "birthday", "joined") VALUES ($1, $2, $3, $4), ($5, $6, $7, $8)""" $ DLIQ.buildQuery q
                              TM.result' q []
                        TS.it "some fields" do
                              let q = insert # into tags name # values [ "my tag", "my other tag" ]
                              TM.parameterized """INSERT INTO "tags"("name") VALUES ($1), ($2)""" $ DLIQ.buildQuery q
                              TM.result' q []
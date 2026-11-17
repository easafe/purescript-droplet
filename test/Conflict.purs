module Test.Conflict where

import Droplet.Language
import Prelude
import Test.Types

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM
import Test.RunTestFailure as TR
import Test.Spec (Spec)
import Test.Spec as TS

tests ∷ Spec Unit
tests = do
      TS.describe "on conflict" do
            TS.describe "invariants" do
                  TS.it "target must match a unique index" do
                        TR.testFailure "conflict-test-1" "NoInstanceFound" "Table users has no unique index on (surname)"
                  TS.it "target must match all columns of a unique index" do
                        TR.testFailure "conflict-test-2" "NoInstanceFound" "Table composite has no unique index on (id)"
                  TS.it "named constraint must exist" do
                        TR.testFailure "conflict-test-3" "NoInstanceFound" "Table unique_values has no unique index named unique_name"
                  TS.it "named constraint must be a unique index" do
                        TR.testFailure "conflict-test-4" "NoInstanceFound" "Table composite has no unique index named sr_user"
                  TS.it "do update requires a conflict target" do
                        TR.testFailure "conflict-test-5" "NoInstanceFound" "ON CONFLICT DO UPDATE requires a conflict target"
                  TS.it "clause must specify an action" do
                        TR.testFailure "conflict-test-6" "NoInstanceFound" "Incomplete ON CONFLICT clause"
                  TS.it "do nothing cannot be filtered" do
                        TR.testFailure "conflict-test-7" "NoInstanceFound" "DO NOTHING does not take a WHERE statement"
                  TS.it "identity columns cannot be updated" do
                        TR.testFailure "conflict-test-8" "NoInstanceFound" "Identity columns cannot be inserted or updated"
                  TS.it "named constraint requires on" do
                        TR.testFailure "conflict-test-11" "TypesDoNotUnify" "On E E"
                  TS.it "columns in where must be qualified" do
                        TR.testFailure "conflict-test-10" "TypesDoNotUnify" "excluded.name"
                  TS.it "excluded columns must be comparable" do
                        TR.testFailure "conflict-test-9" "NoInstanceFound" "ValidComparision"
                  TS.it "constraints must not be repeated" do
                        TR.testFailure "constraint-test-1" "NoInstanceFound" "Constraint Unique declared more than once for column a"
            TS.describe "do nothing" do
                  TS.it "no target" do
                        let q = insert # into users (name /\ surname) # values ("mary" /\ "sue") # on # conflict # do_ # nothing
                        TM.parameterized """INSERT INTO "users" ("name", "surname") VALUES ($1, $2) ON CONFLICT DO NOTHING""" $ DLIQ.buildQuery q
                        TM.result' q []
                  TS.it "single column target" do
                        let q = insert # into maybeKeys id # values 1 # on # conflict id # do_ # nothing # returning id
                        TM.parameterized """INSERT INTO "maybe_keys" ("id") VALUES ($1) ON CONFLICT ("id") DO NOTHING RETURNING "id"""" $ DLIQ.buildQuery q
                        TM.result q []
                  TS.it "composite target" do
                        let q = insert # into uniqueValues (name /\ _by) # values ("named" /\ Just 1) # on # conflict (name /\ _by) # do_ # nothing # returning name
                        TM.parameterized """INSERT INTO "unique_values" ("name", "by") VALUES ($1, $2) ON CONFLICT ("name", "by") DO NOTHING RETURNING "name"""" $ DLIQ.buildQuery q
                        TM.result q []
                  TS.it "composite target column order" do
                        let q = insert # into uniqueValues (name /\ _by) # values ("named" /\ Just 1) # on # conflict (_by /\ name) # do_ # nothing # returning name
                        TM.parameterized """INSERT INTO "unique_values" ("name", "by") VALUES ($1, $2) ON CONFLICT ("by", "name") DO NOTHING RETURNING "name"""" $ DLIQ.buildQuery q
                        TM.result q []
                  TS.it "named constraint" do
                        let q = insert # into uniqueValues (name /\ _by) # values ("named" /\ Just 3) # on # conflict # on # constraint nameUnique # do_ # nothing # returning name
                        TM.parameterized """INSERT INTO "unique_values" ("name", "by") VALUES ($1, $2) ON CONFLICT ON CONSTRAINT "name_unique" DO NOTHING RETURNING "name"""" $ DLIQ.buildQuery q
                        TM.result q []
                  TS.it "composite named constraint" do
                        let q = insert # into uniqueValues (name /\ _by) # values ("named" /\ Just 1) # on # conflict # on # constraint uniquePairs # do_ # nothing # returning name
                        TM.parameterized """INSERT INTO "unique_values" ("name", "by") VALUES ($1, $2) ON CONFLICT ON CONSTRAINT "unique_pairs" DO NOTHING RETURNING "name"""" $ DLIQ.buildQuery q
                        TM.result q []
                  TS.it "default values" do
                        let q = insert # into doublePrimaryKey defaultValues # on # conflict (id /\ secondId) # do_ # nothing
                        TM.notParameterized """INSERT INTO "double_primary_key" DEFAULT VALUES ON CONFLICT ("id", "second_id") DO NOTHING""" $ DLIQ.buildQuery q
                        TM.result' q []
                  TS.it "no conflict" do
                        let q = insert # into uniqueValues (name /\ _by) # values ("other" /\ Just 2) # on # conflict name # do_ # nothing # returning name
                        TM.parameterized """INSERT INTO "unique_values" ("name", "by") VALUES ($1, $2) ON CONFLICT ("name") DO NOTHING RETURNING "name"""" $ DLIQ.buildQuery q
                        TM.result q [ { name: "other" } ]
            TS.describe "do update" do
                  TS.it "set" do
                        let q = insert # into maybeKeys id # values 1 # on # conflict id # do_ # update # set (id .=. 33) # returning id
                        TM.parameterized """INSERT INTO "maybe_keys" ("id") VALUES ($1) ON CONFLICT ("id") DO UPDATE SET id = $2 RETURNING "id"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 33 } ]
                  TS.it "set default" do
                        let q = insert # into users (name /\ surname) # values ("mary" /\ "sue") # on # conflict id # do_ # update # set (birthday .=. Default)
                        TM.parameterized """INSERT INTO "users" ("name", "surname") VALUES ($1, $2) ON CONFLICT ("id") DO UPDATE SET birthday = DEFAULT""" $ DLIQ.buildQuery q
                        TM.result' q []
                  TS.it "set excluded" do
                        let q = insert # into uniqueValues (name /\ _by) # values ("named" /\ Just 5) # on # conflict name # do_ # update # set (_by .=. excluded ... _by) # returning _by
                        TM.parameterized """INSERT INTO "unique_values" ("name", "by") VALUES ($1, $2) ON CONFLICT ("name") DO UPDATE SET by = "excluded"."by" RETURNING "by"""" $ DLIQ.buildQuery q
                        TM.result q [ { by: Just 5 } ]
                  TS.it "set table column" do
                        let q = insert # into uniqueValues (name /\ _by) # values ("named" /\ Just 5) # on # conflict name # do_ # update # set (_by .=. uniqueValues ... _by) # returning _by
                        TM.parameterized """INSERT INTO "unique_values" ("name", "by") VALUES ($1, $2) ON CONFLICT ("name") DO UPDATE SET by = "unique_values"."by" RETURNING "by"""" $ DLIQ.buildQuery q
                        TM.result q [ { by: Just 1 } ]
                  TS.it "many fields" do
                        let q = insert # into uniqueValues (name /\ _by) # values ("named" /\ Just 5) # on # conflict # on # constraint nameUnique # do_ # update # set ((name .=. excluded ... name) /\ (_by .=. Just 9)) # returning _by
                        TM.parameterized """INSERT INTO "unique_values" ("name", "by") VALUES ($1, $2) ON CONFLICT ON CONSTRAINT "name_unique" DO UPDATE SET name = "excluded"."name", by = $3 RETURNING "by"""" $ DLIQ.buildQuery q
                        TM.result q [ { by: Just 9 } ]
                  TS.describe "where" do
                        TS.it "matching" do
                              let q = insert # into uniqueValues (name /\ _by) # values ("named" /\ Just 5) # on # conflict name # do_ # update # set (_by .=. excluded ... _by) # wher (uniqueValues ... _by .<>. excluded ... _by) # returning _by
                              TM.parameterized """INSERT INTO "unique_values" ("name", "by") VALUES ($1, $2) ON CONFLICT ("name") DO UPDATE SET by = "excluded"."by" WHERE "unique_values"."by" <> "excluded"."by" RETURNING "by"""" $ DLIQ.buildQuery q
                              TM.result q [ { by: Just 5 } ]
                        TS.it "not matching" do
                              let q = insert # into uniqueValues (name /\ _by) # values ("named" /\ Just 1) # on # conflict name # do_ # update # set (_by .=. excluded ... _by) # wher (uniqueValues ... _by .<>. excluded ... _by) # returning _by
                              TM.parameterized """INSERT INTO "unique_values" ("name", "by") VALUES ($1, $2) ON CONFLICT ("name") DO UPDATE SET by = "excluded"."by" WHERE "unique_values"."by" <> "excluded"."by" RETURNING "by"""" $ DLIQ.buildQuery q
                              TM.result q []
                        TS.it "parameter" do
                              let q = insert # into uniqueValues (name /\ _by) # values ("named" /\ Just 5) # on # conflict name # do_ # update # set (_by .=. excluded ... _by) # wher (uniqueValues ... name .=. "named") # returning _by
                              TM.parameterized """INSERT INTO "unique_values" ("name", "by") VALUES ($1, $2) ON CONFLICT ("name") DO UPDATE SET by = "excluded"."by" WHERE "unique_values"."name" = $3 RETURNING "by"""" $ DLIQ.buildQuery q
                              TM.result q [ { by: Just 5 } ]
                        TS.it "conditions" do
                              let q = insert # into uniqueValues (name /\ _by) # values ("named" /\ Just 5) # on # conflict name # do_ # update # set (_by .=. excluded ... _by) # wher (uniqueValues ... name .=. excluded ... name .&&. uniqueValues ... _by .<>. excluded ... _by) # returning _by
                              TM.parameterized """INSERT INTO "unique_values" ("name", "by") VALUES ($1, $2) ON CONFLICT ("name") DO UPDATE SET by = "excluded"."by" WHERE ("unique_values"."name" = "excluded"."name" AND "unique_values"."by" <> "excluded"."by") RETURNING "by"""" $ DLIQ.buildQuery q
                              TM.result q [ { by: Just 5 } ]

module Test.Distinct where

import Droplet.Language
import Prelude
import Test.Types
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM
import Test.RunTestFailure as TR

import Test.Spec (Spec)
import Test.Spec as TS

tests ∷ Spec Unit
tests =
      TS.describe "distinct" do
            TS.describe "invariants" do
                  TS.it "must wrap the whole select list" do
                        TR.testFailure "distinct-test-1" "NoInstanceFound" " DISTINCT must follow SELECT"
                  TS.it "cannot be nested" do
                        TR.testFailure "distinct-test-2" "NoInstanceFound" " DISTINCT must follow SELECT"
            TS.it "three columns" do
                  let q = select (distinct (id /\ name /\ surname)) # from users # orderBy id
                  TM.notParameterized """SELECT DISTINCT "id", "name", "surname" FROM "users" ORDER BY "id"""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 1, name: "josh", surname: "j." }, { id: 2, name: "mary", surname: "sue" } ]
            TS.it "field" do
                  let q = select (distinct id) # from messages
                  TM.notParameterized """SELECT DISTINCT "id" FROM "messages"""" $ DLIQ.buildQuery q
                  TM.result' q []
            TS.it "fields" do
                  let q = select (distinct (id /\ name)) # from users
                  TM.notParameterized """SELECT DISTINCT "id", "name" FROM "users"""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 1, name: "josh" }, { id: 2, name: "mary" } ]
            TS.it "qualified fields" do
                  let q = select (distinct (u ... id /\ u ... name)) # from (users # as @"u")
                  TM.notParameterized """SELECT DISTINCT "u"."id" "u.id", "u"."name" "u.name" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                  TM.result q [ { "u.id": 1, "u.name": "josh" }, { "u.id": 2, "u.name": "mary" } ]

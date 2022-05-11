module Test.NakedSelect where

import Droplet.Language
import Prelude
import Test.Types

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM
import Type.Proxy(Proxy(..))
import Test.Spec (Spec)
import Test.Spec as TS


tests ∷ Spec Unit
tests =
      TS.describe "naked select" do
            TS.it "scalar" do
                  let q = select (3 # as n)
                  TM.notParameterized """SELECT 3 AS "n"""" $ DLIQ.buildQuery q
                  TM.result q [ { n: 3 } ]
            TS.describe "function" do
                  TS.it "regular" do
                        let q = select (date_part_age ("year" /\ TM.makeDateTime 2000 1 1) # as u)
                        TM.parameterized """SELECT date_part_age($1, $2) AS "u"""" $ DLIQ.buildQuery q
                        void $ TM.resultOnly q
                  TS.it "side effect" do
                        let q = select (fire_missiles (9 /\ 8) # as u)
                        TM.parameterized """SELECT fire_missiles($1, $2) AS "u"""" $ DLIQ.buildQuery q
                        TM.result q [ { u: unit } ]
                  TS.it "side effect without parameters" do
                        let q = select (random # as u)
                        TM.notParameterized """SELECT random() AS "u"""" $ DLIQ.buildQuery q
                        void $ TM.resultOnly q
            TS.it "subquery" do
                  let q = select (select (34 # as n) # from users # wher (name .=. name) # orderBy id # limit (Proxy :: _ 1))
                  TM.notParameterized """SELECT (SELECT 34 AS "n" FROM "users" WHERE "name" = "name" ORDER BY "id" LIMIT 1)""" $ DLIQ.buildQuery q
                  TM.result q [ { n: Just 34 } ]
            TS.it "aliases" do
                  let q = select (select (u ... id) # from (users # as u) # orderBy id # limit (Proxy :: _ 1))
                  TM.notParameterized """SELECT (SELECT "u"."id" "u.id" FROM "users" AS "u" ORDER BY "id" LIMIT 1)""" $ DLIQ.buildQuery q
                  TM.result q [ { "u.id": Just 1 } ]
            TS.it "named subquery" do
                  let q = select (select (34 # as n) # from users # wher (name .=. name) # orderBy id # limit (Proxy :: _ 1) # as t)
                  TM.notParameterized """SELECT (SELECT 34 AS "n" FROM "users" WHERE "name" = "name" ORDER BY "id" LIMIT 1) AS "t"""" $ DLIQ.buildQuery q
                  TM.result q [ { t: Just 34 } ]
            TS.it "tuple" do
                  let q = select ((3 # as b) /\ (select name # from users # orderBy name # limit (Proxy :: _ 1)) /\ (select (u ... name # as n) # from (users # as u) # orderBy (name # desc) # limit (Proxy :: _ 1)))
                  TM.notParameterized """SELECT 3 AS "b", (SELECT "name" FROM "users" ORDER BY "name" LIMIT 1), (SELECT "u"."name" AS "n" FROM "users" AS "u" ORDER BY "name" DESC LIMIT 1)""" $ DLIQ.buildQuery q
                  TM.result q [ { b: 3, name: Just "josh", n: Just "mary" } ]

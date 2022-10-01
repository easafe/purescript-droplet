module Test.Function where

import Prelude hiding (join)

import Data.BigInt as DB
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Language
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM
import Test.Types

import Test.Spec (Spec)
import Test.Spec as TS

tests ∷ Spec Unit
tests = TS.describe "functions" do
      TS.describe "user defined functions" do
            TS.it "value" do
                  let q = select (date_part_age ("year" /\ (TM.makeDateTime 1900 2 2)) # as u) # from users
                  TM.parameterized """SELECT date_part_age($1, $2) AS "u" FROM "users"""" $ DLIQ.buildQuery q
                  void $ TM.resultOnly q
            TS.it "field" do
                  let q = select (date_part_age ("month" /\ date) # as u) # from messages
                  TM.parameterized """SELECT date_part_age($1, "date") AS "u" FROM "messages"""" $ DLIQ.buildQuery q
                  void $ TM.resultOnly q
      TS.describe "coalesce" do
            TS.it "scalar" do
                  let q = select (coalesce (3 /\ 4) # as u) # from users
                  TM.parameterized """SELECT coalesce($1, $2) AS "u" FROM "users"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just 3 }, { u: Just 3 } ]
            TS.it "field" do
                  let q = select (coalesce (_by /\ _by) # as u) # from tags
                  TM.notParameterized """SELECT coalesce("by", "by") AS "u" FROM "tags"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just 1 } ]
            TS.it "path" do
                  let q = select (coalesce (u ... id /\ u ... id) # as u) # from (users # as u)
                  TM.notParameterized """SELECT coalesce("u"."id", "u"."id") AS "u" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just 1 }, { u: Just 2 } ]
            TS.it "mixed" do
                  let q = select (coalesce (id /\ u ... id /\ 4) # as u) # from (users # as u)
                  TM.parameterized """SELECT coalesce("id", "u"."id", $1) AS "u" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just 1 }, { u: Just 2 } ]
      TS.it "function as argument" do
            let q = select (date_part_age ("year" /\ coalesce (date /\ utc_now)) # as u) # from messages
            TM.parameterized """SELECT date_part_age($1, coalesce("date", utc_now())) AS "u" FROM "messages"""" $ DLIQ.buildQuery q
            void $ TM.resultOnly q
      TS.describe "count" do
            TS.it "star" do
                  let q = select (count star # as u) # from users
                  TM.notParameterized """SELECT count(*) AS "u" FROM "users"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: DB.fromInt 2 } ]
            TS.it "field" do
                  let q = select (count id # as u) # from users
                  TM.notParameterized """SELECT count("id") AS "u" FROM "users"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: DB.fromInt 2 } ]
            TS.it "path" do
                  let q = select (count (u ... id) # as u) # from (users # as u)
                  TM.notParameterized """SELECT count("u"."id") AS "u" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: DB.fromInt 2 } ]
      TS.it "function without parameters" do
            let q = select ((random # as u) /\ id) # from users
            TM.notParameterized """SELECT random() AS "u", "id" FROM "users"""" $ DLIQ.buildQuery q
            void $ TM.resultOnly q
      TS.describe "string_agg" do
            TS.it "field" do
                  let q = select (string_agg name ", " # as u) # from users
                  TM.parameterized """SELECT string_agg("name", $1) AS "u" FROM "users"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just "josh, mary" } ]
            TS.it "path" do
                  let q = select (string_agg (u ... name) ", " # as u) # from (users # as u)
                  TM.parameterized """SELECT string_agg("u"."name", $1) AS "u" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just "josh, mary" } ]
            TS.describe "order by" do
                  TS.it "field" do
                        let q = select (string_agg name (", " # orderBy id) # as u) # from users
                        TM.parameterized """SELECT string_agg("name", $1 ORDER BY "id") AS "u" FROM "users"""" $ DLIQ.buildQuery q
                        TM.result q [ { u: Just "josh, mary" } ]
                  TS.it "path" do
                        let q = select (string_agg (u ... name) (", " # orderBy (u ... id)) # as u) # from (users # as u)
                        TM.parameterized """SELECT string_agg("u"."name", $1 ORDER BY "u"."id") AS "u" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                        TM.result q [ { u: Just "josh, mary" } ]
      TS.describe "int_agg" do
            TS.it "field" do
                  let q = select (array_agg name # as u) # from users
                  TM.notParameterized """SELECT array_agg("name") AS "u" FROM "users"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just [ "josh", "mary" ] } ]
            TS.it "path" do
                  let q = select (array_agg (u ... id) # as u) # from (users # as u)
                  TM.notParameterized """SELECT array_agg("u"."id") AS "u" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just [1, 2] } ]
            TS.it "joined source" do
                  let q = select (array_agg (u ... id) # as u) # from (join (users # as u) (messages # as t) # on (u ... id .=. t ... id))
                  TM.notParameterized """SELECT array_agg("u"."id") AS "u" FROM "users" AS "u" INNER JOIN "messages" AS "t" ON "u"."id" = "t"."id"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just [1, 2] } ]
            TS.describe "order by" do
                  TS.it "field" do
                        let q = select (array_agg (id # orderBy id) # as u) # from users
                        TM.notParameterized """SELECT array_agg("id" ORDER BY "id") AS "u" FROM "users"""" $ DLIQ.buildQuery q
                        TM.result q [ { u: Just [1, 2]} ]
                  TS.it "path" do
                        let q = select (array_agg (u ... name # orderBy (u ... id)) # as u) # from (users # as u)
                        TM.notParameterized """SELECT array_agg("u"."name" ORDER BY "u"."id") AS "u" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                        TM.result q [ { u: Just ["josh", "mary"] } ]

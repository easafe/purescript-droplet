module Test.Function where

import Prelude

import Data.BigInt as DB
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Language (as, coalesce, count, from, orderBy, random, select, star, string_agg, (...))
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM
import Test.Types
import Test.Unit (TestSuite)
import Test.Unit as TU

tests ∷ TestSuite
tests = TU.suite "functions" do
      TU.suite "user defined functions" do
            TU.test "value" do
                  let q = select (date_part_age ("year" /\ (TM.makeDateTime 1900 2 2)) # as u) # from users
                  TM.parameterized """SELECT date_part_age($1, $2) AS "u" FROM "users"""" $ DLIQ.buildQuery q
                  void $ TM.resultOnly q
            TU.test "field" do
                  let q = select (date_part_age ("month" /\ date) # as u) # from messages
                  TM.parameterized """SELECT date_part_age($1, "date") AS "u" FROM "messages"""" $ DLIQ.buildQuery q
                  void $ TM.resultOnly q
      TU.suite "coalesce" do
            TU.test "scalar" do
                  let q = select (coalesce (3 /\ 4) # as u) # from users
                  TM.parameterized """SELECT coalesce($1, $2) AS "u" FROM "users"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just 3 }, { u: Just 3 } ]
            TU.test "field" do
                  let q = select (coalesce (_by /\ _by) # as u) # from tags
                  TM.notParameterized """SELECT coalesce("by", "by") AS "u" FROM "tags"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just 1 } ]
            TU.test "path" do
                  let q = select (coalesce (u ... id /\ u ... id) # as u) # from (users # as u)
                  TM.notParameterized """SELECT coalesce("u"."id", "u"."id") AS "u" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just 1 }, { u: Just 2 } ]
            TU.test "mixed" do
                  let q = select (coalesce (id /\ u ... id /\ 4) # as u) # from (users # as u)
                  TM.parameterized """SELECT coalesce("id", "u"."id", $1) AS "u" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just 1 }, { u: Just 2 } ]
      TU.test "function as argument" do
            let q = select (date_part_age ("year" /\ coalesce (date /\ utc_now)) # as u) # from messages
            TM.parameterized """SELECT date_part_age($1, coalesce("date", utc_now())) AS "u" FROM "messages"""" $ DLIQ.buildQuery q
            void $ TM.resultOnly q
      TU.suite "count" do
            TU.test "star" do
                  let q = select (count star # as u) # from users
                  TM.notParameterized """SELECT count(*) AS "u" FROM "users"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: DB.fromInt 2 } ]
            TU.test "field" do
                  let q = select (count id # as u) # from users
                  TM.notParameterized """SELECT count("id") AS "u" FROM "users"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: DB.fromInt 2 } ]
            TU.test "path" do
                  let q = select (count (u ... id) # as u) # from (users # as u)
                  TM.notParameterized """SELECT count("u"."id") AS "u" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: DB.fromInt 2 } ]
      TU.test "function without parameters" do
            let q = select ((random # as u) /\ id) # from users
            TM.notParameterized """SELECT random() AS "u", "id" FROM "users"""" $ DLIQ.buildQuery q
            void $ TM.resultOnly q
      TU.suite "string_agg" do
            TU.test "field" do
                  let q = select (string_agg name ", " # as u) # from users
                  TM.parameterized """SELECT string_agg("name", $1) AS "u" FROM "users"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just "josh, mary" } ]
            TU.test "path" do
                  let q = select (string_agg (u ... name) ", " # as u) # from (users # as u)
                  TM.parameterized """SELECT string_agg("u"."name", $1) AS "u" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                  TM.result q [ { u: Just "josh, mary" } ]
            TU.suite "order by" do
                  TU.test "field" do
                        let q = select (string_agg name (", " # orderBy id) # as u) # from users
                        TM.parameterized """SELECT string_agg("name", $1 ORDER BY "id") AS "u" FROM "users"""" $ DLIQ.buildQuery q
                        TM.result q [ { u: Just "josh, mary" } ]
                  TU.test "path" do
                        let q = select (string_agg (u ... name) (", " # orderBy (u ... id)) # as u) # from (users # as u)
                        TM.parameterized """SELECT string_agg("u"."name", $1 ORDER BY "u"."id") AS "u" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                        TM.result q [ { u: Just "josh, mary" } ]

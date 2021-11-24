module Test.GroupBy where

import Droplet.Language (as, count, from, groupBy, orderBy, select, (...))
import Prelude
import Test.Types (b, id, name, u, users)

import Data.BigInt as BG
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Gen as DLIQ
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests ∷ TestSuite
tests =
      TU.suite "group by" do
            TU.test "single field" do
                  let q = select id # from users # groupBy id # orderBy id
                  TM.notParameterized """SELECT "id" FROM "users" GROUP BY "id" ORDER BY "id"""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 1 }, { id: 2 } ]
            TU.test "many fields" do
                  let q = select ((count id # as b) /\ name) # from users # groupBy (id /\ name) # orderBy id
                  TM.notParameterized """SELECT count("id") AS "b", "name" FROM "users" GROUP BY "id", "name" ORDER BY "id"""" $ DLIQ.buildQuery q
                  TM.result q [ { b: BG.fromInt 1, name: "josh" }, { b: BG.fromInt 1, name: "mary" } ]
            TU.suite "path" do
                  TU.test "single field" do
                        let q = select id # from (select id # from users # as u) # groupBy (u ... id) # orderBy id
                        TM.notParameterized """SELECT "id" FROM (SELECT "id" FROM "users") AS "u" GROUP BY "u"."id" ORDER BY "id"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 1 }, { id: 2 } ]
                  TU.test "many fields" do
                        let q = select (id /\ u ... name) # from (users # as u) # groupBy (u ... name /\ id) # orderBy id
                        TM.notParameterized """SELECT "id", "u"."name" "u.name" FROM "users" AS "u" GROUP BY "u"."name", "id" ORDER BY "id"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 1, "u.name": "josh" }, { id: 2, "u.name": "mary" } ]

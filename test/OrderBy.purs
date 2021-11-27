module Test.OrderBy where

import Data.Tuple.Nested ((/\))
import Droplet.Language
import Droplet.Language.Internal.Translate as DLIQ
import Prelude (discard, void, (#), ($))
import Test.Model as TM
import Test.Types (date_part_age, id, date, messages, n, name, t, u, users)
import Test.Unit (TestSuite)
import Test.Unit as TU

tests ∷ TestSuite
tests =
      TU.suite "order by" do
            TU.test "projection" do
                  let q = select (4 # as n) # from users # orderBy n
                  TM.notParameterized """SELECT 4 AS "n" FROM "users" ORDER BY "n"""" $ DLIQ.buildQuery q
                  TM.result q [ { n: 4 }, { n: 4 } ]
            TU.test "field name" do
                  let q = select id # from users # wher (id .<>. 10) # orderBy (id /\ name)
                  TM.parameterized """SELECT "id" FROM "users" WHERE "id" <> $1 ORDER BY "id", "name"""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 1 }, { id: 2 } ]
            TU.test "asc" do
                  let q = select (id # as n) # from users # wher (id .<>. 4) # orderBy (n /\ (name # asc))
                  TM.parameterized """SELECT "id" AS "n" FROM "users" WHERE "id" <> $1 ORDER BY "n", "name" ASC""" $ DLIQ.buildQuery q
                  TM.result q [ { n: 1 }, { n: 2 } ]
            TU.test "desc" do
                  let q = select id # from users # orderBy (id # desc)
                  TM.notParameterized """SELECT "id" FROM "users" ORDER BY "id" DESC""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 2 }, { id: 1 } ]
            TU.test "distinct" do
                  let q = select (distinct (id # as n)) # from users # orderBy n
                  TM.notParameterized """SELECT DISTINCT "id" AS "n" FROM "users" ORDER BY "n"""" $ DLIQ.buildQuery q
                  TM.result q [ { n: 1 }, { n: 2 } ]
            TU.suite "function" do
                  TU.test "regular" do
                        let q = select (4 # as n) # from users # orderBy (date_part_age ("year" /\ TM.makeDateTime 2000 1 1))
                        TM.parameterized """SELECT 4 AS "n" FROM "users" ORDER BY date_part_age($1, $2)""" $ DLIQ.buildQuery q
                        TM.result q [ { n: 4 }, { n: 4 } ]
                  TU.test "no parameters" do
                        let q = select (4 # as n) # from users # orderBy random
                        TM.notParameterized """SELECT 4 AS "n" FROM "users" ORDER BY random()""" $ DLIQ.buildQuery q
                        void $ TM.resultOnly q
            TU.suite "path" do
                  TU.test "field name" do
                        let q = select id # from (users # as u) # orderBy (u ... id)
                        TM.notParameterized """SELECT "id" FROM "users" AS "u" ORDER BY "u"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 1 }, { id: 2 } ]
                  TU.test "asc" do
                        let q = select id # from (select id # from users # as u) # orderBy (u ... id # asc)
                        TM.notParameterized """SELECT "id" FROM (SELECT "id" FROM "users") AS "u" ORDER BY "u"."id" ASC""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 1 }, { id: 2 } ]
                  TU.test "desc" do
                        let q = select (3 # as id) # from (join (users # as u) (messages # as t) # on (t ... id .=. u ... id)) # orderBy (u ... id # desc)
                        TM.notParameterized """SELECT 3 AS "id" FROM "users" AS "u" INNER JOIN "messages" AS "t" ON "t"."id" = "u"."id" ORDER BY "u"."id" DESC""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 3 }, { id: 3 } ]
                  TU.test "function" do
                        let q = select (4 # as n) # from (messages # as u) # orderBy (date_part_age ("year" /\ u ... date))
                        TM.parameterized """SELECT 4 AS "n" FROM "messages" AS "u" ORDER BY date_part_age($1, "u"."date")""" $ DLIQ.buildQuery q
                        TM.result q [ { n: 4 }, { n: 4 } ]

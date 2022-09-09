module Test.Where where

import Droplet.Language
import Prelude hiding (not, join)
import Test.Types

import Data.Array.NonEmpty as DAN
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM
import Test.Spec (Spec)
import Test.Spec as TS
import Type.Proxy (Proxy(..))

tests ∷ Spec Unit
tests = do
      TS.describe "where" do
            TS.describe "compared to parameter" do
                  TS.it "equals" do
                        let q = select recipient # from messages # wher (sender .=. 1)
                        TM.parameterized """SELECT "recipient" FROM "messages" WHERE "sender" = $1""" $ DLIQ.buildQuery q
                        TM.result q [ { recipient: 2 } ]
                  TS.it "not equals" do
                        let q = select sender # from messages # wher (recipient .<>. 2)
                        TM.parameterized """SELECT "sender" FROM "messages" WHERE "recipient" <> $1""" $ DLIQ.buildQuery q
                        TM.result q [ { sender: 2 } ]
                  TS.it "lesser than" do
                        let q = select sender # from messages # wher (recipient .<. 2)
                        TM.parameterized """SELECT "sender" FROM "messages" WHERE "recipient" < $1""" $ DLIQ.buildQuery q
                        TM.result q [ { sender: 2 } ]
                  TS.it "lesser equals than" do
                        let q = select sender # from messages # wher (recipient .<=. 2)
                        TM.parameterized """SELECT "sender" FROM "messages" WHERE "recipient" <= $1""" $ DLIQ.buildQuery q
                        TM.result q [ { sender: 1}, { sender: 2 } ]
                  TS.it "nullable" do
                        let q = select _by # from tags # wher (_by .=. 2)
                        TM.parameterized """SELECT "by" FROM "tags" WHERE "by" = $1""" $ DLIQ.buildQuery q
                        TM.result q [  ]

            TS.describe "compared to field" do
                  TS.it "equals" do
                        let q = select (34 # as n) # from users # wher (name .=. surname)
                        TM.notParameterized """SELECT 34 AS "n" FROM "users" WHERE "name" = "surname"""" $ DLIQ.buildQuery q
                        TM.result q []
                  TS.it "not equals" do
                        let q = select (34 # as n) # from users # wher (name .<>. surname)
                        TM.notParameterized """SELECT 34 AS "n" FROM "users" WHERE "name" <> "surname"""" $ DLIQ.buildQuery q
                        TM.result q [ { n: 34 }, { n: 34 } ]
                  TS.it "greater than" do
                        let q = select sender # from messages # wher (recipient .>. 2)
                        TM.parameterized """SELECT "sender" FROM "messages" WHERE "recipient" > $1""" $ DLIQ.buildQuery q
                        TM.result q []
                  TS.it "greater equals than" do
                        let q = select sender # from messages # wher (recipient .>=. 2)
                        TM.parameterized """SELECT "sender" FROM "messages" WHERE "recipient" >= $1""" $ DLIQ.buildQuery q
                        TM.result q [{sender: 1}]

            TS.describe "logical operands" do
                  TS.describe "and" do
                        TS.it "single" do
                              let q = select id # from users # wher (name .=. "josh" .&&. name .<>. surname)
                              TM.parameterized """SELECT "id" FROM "users" WHERE ("name" = $1 AND "name" <> "surname")""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 1 } ]
                        TS.it "many" do
                              let q = select id # from users # wher (name .=. "josh" .&&. "josh" .=. name .&&. surname .=. "j.")
                              TM.parameterized """SELECT "id" FROM "users" WHERE (("name" = $1 AND $2 = "name") AND "surname" = $3)""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 1 } ]

                  TS.describe "or" do
                        TS.it "single" do
                              let q = select id # from users # wher (name .=. "mary" .||. name .=. surname)
                              TM.parameterized """SELECT "id" FROM "users" WHERE ("name" = $1 OR "name" = "surname")""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 2 } ]
                        TS.it "many" do
                              let q = select id # from users # wher (name .=. "josh" .||. name .=. "j." .||. surname .<>. "josh")
                              TM.parameterized """SELECT "id" FROM "users" WHERE (("name" = $1 OR "name" = $2) OR "surname" <> $3)""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 1 }, { id: 2 } ]

                  TS.it "in" do
                        let q = select id # from users # wher (id `in_` DAN.fromNonEmpty (NonEmpty 3 [4, 5]))
                        TM.parameterized """SELECT "id" FROM "users" WHERE "id" IN ($1, $2, $3)""" $ DLIQ.buildQuery q
                        TM.result q []

                  TS.describe "is null" do
                        TS.it "maybe field" do
                              let q = select id # from tags # wher (_by # isNull)
                              TM.notParameterized """SELECT "id" FROM "tags" WHERE "by" IS NULL""" $ DLIQ.buildQuery q
                              TM.result q [  ]
                        TS.it "joined field" do
                              let q = select joined # from (leftJoin users (tags # as t) # on (joined .=. created)) # wher (isNull (t ... id))
                              TM.notParameterized """SELECT "joined" FROM "users" LEFT JOIN "tags" AS "t" ON "joined" = "created" WHERE "t"."id" IS NULL""" $ DLIQ.buildQuery q
                              TM.result' q []

                  TS.describe "is not null" do
                        TS.it "maybe field" do
                              let q = select id # from tags # wher (_by # isNotNull)
                              TM.notParameterized """SELECT "id" FROM "tags" WHERE "by" IS NOT NULL""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 1 } ]
                        TS.it "joined field" do
                              let q = select joined # from (leftJoin users (tags # as t) # on (joined .=. created)) # wher (isNotNull (t ... id))
                              TM.notParameterized """SELECT "joined" FROM "users" LEFT JOIN "tags" AS "t" ON "joined" = "created" WHERE "t"."id" IS NOT NULL""" $ DLIQ.buildQuery q
                              TM.result q [  ]

                  TS.describe "not" do
                        TS.it "operator" do
                              let q = select id # from users # wher (not (id .<>. 5))
                              TM.parameterized """SELECT "id" FROM "users" WHERE NOT "id" <> $1""" $ DLIQ.buildQuery q
                              TM.result q []
                        TS.it "and" do
                              let q = select id # from (users # as u) # wher (not (id .<>. 5) .&&. id .=. 1)
                              TM.parameterized """SELECT "id" FROM "users" AS "u" WHERE (NOT "id" <> $1 AND "id" = $2)""" $ DLIQ.buildQuery q
                              TM.result q []
                        TS.it "or" do
                              let q = select id # from (users # as u) # wher (not (id .<>. 5 .||. id .=. 1))
                              TM.parameterized """SELECT "id" FROM "users" AS "u" WHERE NOT ("id" <> $1 OR "id" = $2)""" $ DLIQ.buildQuery q
                              TM.result q []
                        TS.it "exists" do
                              let q = select id # from (users # as u) # wher (not $ exists $ select id # from users)
                              TM.notParameterized """SELECT "id" FROM "users" AS "u" WHERE NOT EXISTS (SELECT "id" FROM "users")""" $ DLIQ.buildQuery q
                              TM.result q []

                  TS.describe "exists" do
                        TS.it "column" do
                              let q = select id # from users # wher (exists $ select id # from users)
                              TM.notParameterized """SELECT "id" FROM "users" WHERE EXISTS (SELECT "id" FROM "users")""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 1 }, { id: 2 } ]
                        TS.describe "path" do
                              TS.it "outer" do
                                    let q = select id # from (users # as u) # wher (exists $ select (u ... id) # from users)
                                    TM.notParameterized """SELECT "id" FROM "users" AS "u" WHERE EXISTS (SELECT "u"."id" "u.id" FROM "users")""" $ DLIQ.buildQuery q
                                    TM.result q [ { id: 1 }, { id: 2 } ]
                              TS.it "outer where" do
                                    let q = select id # from (users # as u) # wher (exists $ select (3 # as t) # from users # wher (id .=. u ... id))
                                    TM.notParameterized """SELECT "id" FROM "users" AS "u" WHERE EXISTS (SELECT 3 AS "t" FROM "users" WHERE "id" = "u"."id")""" $ DLIQ.buildQuery q
                                    TM.result q [ { id: 1 }, { id: 2 } ]
                              TS.it "outer inner where" do
                                    let q = select id # from (users # as u) # wher (exists $ select id # from (users # as t) # wher (t ... id .=. u ... id))
                                    TM.notParameterized """SELECT "id" FROM "users" AS "u" WHERE EXISTS (SELECT "id" FROM "users" AS "t" WHERE "t"."id" = "u"."id")""" $ DLIQ.buildQuery q
                                    TM.result q [ { id: 1 }, { id: 2 } ]

                  TS.describe "mixed" do
                        TS.it "not bracketed" do
                              let q = select id # from users # wher (id .=. 333 .||. id .=. 33 .&&. id .=. 3)
                              TM.parameterized """SELECT "id" FROM "users" WHERE ("id" = $1 OR ("id" = $2 AND "id" = $3))""" $ DLIQ.buildQuery q
                              TM.result q []
                        TS.it "bracketed" do
                              let q = select id # from users # wher ((id .=. 2 .||. id .=. 22) .&&. id .=. 2)
                              TM.parameterized """SELECT "id" FROM "users" WHERE (("id" = $1 OR "id" = $2) AND "id" = $3)""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 2 } ]
                        TS.it "with exists" do
                              let q = select id # from users # wher ((id .=. 2 .||. (exists $ select id # from users)) .&&. id .=. 2)
                              TM.parameterized """SELECT "id" FROM "users" WHERE (("id" = $1 OR EXISTS (SELECT "id" FROM "users")) AND "id" = $2)""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 2 } ]
                        TS.it "with not" do
                              let q = select id # from users # wher ((id .=. 2 .||. not (exists $ select id # from users)) .&&. not (id .=. 2))
                              TM.parameterized """SELECT "id" FROM "users" WHERE (("id" = $1 OR NOT EXISTS (SELECT "id" FROM "users")) AND NOT "id" = $2)""" $ DLIQ.buildQuery q
                              TM.result q []

            TS.describe "subqueries" do
                  TS.it "scalar" do
                        let namep = "mary"
                        let q = select (select (4 # as n) # from users # wher (name .=. namep) # orderBy id # limit (Proxy :: _ 1) # as b)
                        TM.parameterized """SELECT (SELECT 4 AS "n" FROM "users" WHERE "name" = $1 ORDER BY "id" LIMIT 1) AS "b"""" $ DLIQ.buildQuery q
                        TM.result q [ { b: Just 4 } ]
                  TS.it "field" do
                        let namep = "josh"
                        let q = select (select id # from users # wher (name .=. namep) # orderBy id # limit (Proxy :: _ 1) # as b)
                        TM.parameterized """SELECT (SELECT "id" FROM "users" WHERE "name" = $1 ORDER BY "id" LIMIT 1) AS "b"""" $ DLIQ.buildQuery q
                        TM.result q [ { b: Just 1 } ]
                  TS.it "tuple" do
                        let parameters = { d: "mary", e: 2 }
                        let q = select ((3 # as (Proxy ∷ Proxy "e")) /\ (select id # from users # wher (name .=. parameters.d) # orderBy id # limit (Proxy :: _ 1) # as b) /\ (select id # from messages # wher (id .=. parameters.e) # orderBy id # limit (Proxy :: _ 1) # as n))
                        TM.parameterized """SELECT 3 AS "e", (SELECT "id" FROM "users" WHERE "name" = $1 ORDER BY "id" LIMIT 1) AS "b", (SELECT "id" FROM "messages" WHERE "id" = $2 ORDER BY "id" LIMIT 1) AS "n"""" $ DLIQ.buildQuery q
                        TM.result q [ { e: 3, b: Just 2, n: Just 2 } ]
                  TS.it "where" do
                        let parameters = { d: "mary", e: 2 }
                        let q = select ((3 # as (Proxy ∷ Proxy "e")) /\ (select id # from users # wher (name .=. parameters.d) # orderBy id # limit (Proxy :: _ 1) # as b) /\ (select id # from messages # wher (id .=. parameters.e) # orderBy id # limit (Proxy :: _ 1) # as n)) # from users # wher (id .=. 1 .||. id .=. 2)
                        TM.parameterized """SELECT 3 AS "e", (SELECT "id" FROM "users" WHERE "name" = $1 ORDER BY "id" LIMIT 1) AS "b", (SELECT "id" FROM "messages" WHERE "id" = $2 ORDER BY "id" LIMIT 1) AS "n" FROM "users" WHERE ("id" = $3 OR "id" = $4)""" $ DLIQ.buildQuery q
                        TM.result q [ { e: 3, b: Just 2, n: Just 2 }, { e: 3, b: Just 2, n: Just 2 } ]

            TS.describe "references" do
                  TS.it "named table" do
                        let q = select recipient # from (messages # as u) # wher (sender .=. 1)
                        TM.parameterized """SELECT "recipient" FROM "messages" AS "u" WHERE "sender" = $1""" $ DLIQ.buildQuery q
                        TM.result q [ { recipient: 2 } ]
                  TS.it "named table alias" do
                        let q = select (u ... id) # from (users # as u) # wher (u ... id .<>. 3)
                        TM.parameterized """SELECT "u"."id" "u.id" FROM "users" AS "u" WHERE "u"."id" <> $1""" $ DLIQ.buildQuery q
                        TM.result q [ { "u.id": 1 }, { "u.id": 2 } ]
                  TS.it "named subquery" do
                        let q = select n # from (select (id # as n) # from users # as u) # wher (u ... n .<>. 3)
                        TM.parameterized """SELECT "n" FROM (SELECT "id" AS "n" FROM "users") AS "u" WHERE "u"."n" <> $1""" $ DLIQ.buildQuery q
                        TM.result q [ { n: 1 }, { n: 2 } ]
                  TS.describe "column subquery" do
                        TS.it "outer" do
                              let q = select (select id # from users # wher (u ... id .<>. id) # orderBy id # limit (Proxy :: _ 1)) # from (users # as u)
                              TM.notParameterized """SELECT (SELECT "id" FROM "users" WHERE "u"."id" <> "id" ORDER BY "id" LIMIT 1) FROM "users" AS "u"""" $ DLIQ.buildQuery q
                              TM.result q [ { id: Just 2 }, { id: Just 1 } ]
                        TS.it "value" do
                              let q = select (id /\ (select ((u ... id) # as n) # from users # wher (id .=. 2) # orderBy id # limit (Proxy :: _ 1))) # from (users # as u)
                              TM.parameterized """SELECT "id", (SELECT "u"."id" AS "n" FROM "users" WHERE "id" = $1 ORDER BY "id" LIMIT 1) FROM "users" AS "u"""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 1, n: Just 1 }, { id: 2, n: Just 2 } ]
                        TS.it "inner alias" do
                              let q = select (select (n ... name) # from (users # as n) # wher (u ... id .<>. n ... id) # orderBy id # limit (Proxy :: _ 1)) # from (users # as u)
                              TM.notParameterized """SELECT (SELECT "n"."name" "n.name" FROM "users" AS "n" WHERE "u"."id" <> "n"."id" ORDER BY "id" LIMIT 1) FROM "users" AS "u"""" $ DLIQ.buildQuery q
                              TM.result q [ { "n.name": Just "mary" }, { "n.name": Just "josh" } ]
                        TS.it "same alias" do
                              let q = select (id /\ (select (u ... id) # from (users # as u) # wher (u ... id .<>. u ... id) # orderBy id # limit (Proxy :: _ 1))) # from (users # as u)
                              TM.notParameterized """SELECT "id", (SELECT "u"."id" "u.id" FROM "users" AS "u" WHERE "u"."id" <> "u"."id" ORDER BY "id" LIMIT 1) FROM "users" AS "u"""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 1, "u.id": Nothing }, { id: 2, "u.id": Nothing } ]
                        TS.it "join" do
                              let q = select ((select name # from users # wher (id .=. u ... _by) # orderBy id # limit (Proxy :: _ 1)) # as b) # from (join (tags # as u) (messages # as b) # on (u ... id .=. b ... id)) # wher (u ... id .=. 34)
                              TM.parameterized """SELECT (SELECT "name" FROM "users" WHERE "id" = "u"."by" ORDER BY "id" LIMIT 1) AS "b" FROM "tags" AS "u" INNER JOIN "messages" AS "b" ON "u"."id" = "b"."id" WHERE "u"."id" = $1""" $ DLIQ.buildQuery q
                              TM.result q []
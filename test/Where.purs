module Test.Where where

import Droplet.Language
import Prelude hiding (not, join)
import Test.Types

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Query as DLIQ
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU
import Type.Proxy (Proxy(..))

tests ∷ TestSuite
tests = do
      TU.suite "where" do
            TU.suite "compared to parameter" do
                  TU.test "equals" do
                        let q = select recipient # from messages # wher (sender .=. 1)
                        TM.parameterized """SELECT "recipient" FROM messages WHERE "sender" = $1""" $ DLIQ.buildQuery q
                        TM.result q [ { recipient: 2 } ]
                  TU.test "not equals" do
                        let q = select sender # from messages # wher (recipient .<>. 2)
                        TM.parameterized """SELECT "sender" FROM messages WHERE "recipient" <> $1""" $ DLIQ.buildQuery q
                        TM.result q [ { sender: 2 } ]
                  TU.test "lesser than" do
                        let q = select sender # from messages # wher (recipient .<. 2)
                        TM.parameterized """SELECT "sender" FROM messages WHERE "recipient" < $1""" $ DLIQ.buildQuery q
                        TM.result q [ { sender: 2 } ]

            TU.suite "compared to field" do
                  TU.test "equals" do
                        let q = select (34 # as n) # from users # wher (name .=. surname)
                        TM.notParameterized """SELECT 34 AS "n" FROM users WHERE "name" = "surname"""" $ DLIQ.buildQuery q
                        TM.result q []
                  TU.test "not equals" do
                        let q = select (34 # as n) # from users # wher (name .<>. surname)
                        TM.notParameterized """SELECT 34 AS "n" FROM users WHERE "name" <> "surname"""" $ DLIQ.buildQuery q
                        TM.result q [ { n: 34 }, { n: 34 } ]
                  TU.test "greater than" do
                        let q = select sender # from messages # wher (recipient .>. 2)
                        TM.parameterized """SELECT "sender" FROM messages WHERE "recipient" > $1""" $ DLIQ.buildQuery q
                        TM.result q []

            TU.suite "logical operands" do
                  TU.suite "and" do
                        TU.test "single" do
                              let q = select id # from users # wher (name .=. "josh" .&&. name .<>. surname)
                              TM.parameterized """SELECT "id" FROM users WHERE ("name" = $1 AND "name" <> "surname")""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 1 } ]
                        TU.test "many" do
                              let q = select id # from users # wher (name .=. "josh" .&&. "josh" .=. name .&&. surname .=. "j.")
                              TM.parameterized """SELECT "id" FROM users WHERE (("name" = $1 AND $2 = "name") AND "surname" = $3)""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 1 } ]

                  TU.suite "or" do
                        TU.test "single" do
                              let q = select id # from users # wher (name .=. "mary" .||. name .=. surname)
                              TM.parameterized """SELECT "id" FROM users WHERE ("name" = $1 OR "name" = "surname")""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 2 } ]
                        TU.test "many" do
                              let q = select id # from users # wher (name .=. "josh" .||. name .=. "j." .||. surname .<>. "josh")
                              TM.parameterized """SELECT "id" FROM users WHERE (("name" = $1 OR "name" = $2) OR "surname" <> $3)""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 1 }, { id: 2 } ]

                  TU.test "in" do
                        let q = select id # from users # wher (id `in_` [ 3, 4, 5 ])
                        TM.parameterized """SELECT "id" FROM users WHERE "id" IN ($1, $2, $3)""" $ DLIQ.buildQuery q
                        TM.result q []

                  TU.suite "is not null" do
                        TU.test "maybe field" do
                              let q = select id # from tags # wher (_by # isNotNull)
                              TM.notParameterized """SELECT "id" FROM tags WHERE "by" IS NOT NULL""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 1 } ]
                        TU.test "joined field" do
                              let q = select joined # from (leftJoin users (tags # as t) # on (joined .=. created)) # wher (isNotNull (t ... id))
                              TM.notParameterized """SELECT "joined" FROM users LEFT JOIN tags AS "t" ON "joined" = "created" WHERE "t"."id" IS NOT NULL""" $ DLIQ.buildQuery q
                              TM.result q [  ]

                  TU.suite "not" do
                        TU.test "operator" do
                              let q = select id # from users # wher (not (id .<>. 5))
                              TM.parameterized """SELECT "id" FROM users WHERE NOT "id" <> $1""" $ DLIQ.buildQuery q
                              TM.result q []
                        TU.test "and" do
                              let q = select id # from (users # as u) # wher (not (id .<>. 5) .&&. id .=. 1)
                              TM.parameterized """SELECT "id" FROM users AS "u" WHERE (NOT "id" <> $1 AND "id" = $2)""" $ DLIQ.buildQuery q
                              TM.result q []
                        TU.test "or" do
                              let q = select id # from (users # as u) # wher (not (id .<>. 5 .||. id .=. 1))
                              TM.parameterized """SELECT "id" FROM users AS "u" WHERE NOT ("id" <> $1 OR "id" = $2)""" $ DLIQ.buildQuery q
                              TM.result q []
                        TU.test "exists" do
                              let q = select id # from (users # as u) # wher (not $ exists $ select id # from users)
                              TM.notParameterized """SELECT "id" FROM users AS "u" WHERE NOT EXISTS (SELECT "id" FROM users)""" $ DLIQ.buildQuery q
                              TM.result q []

                  TU.suite "exists" do
                        TU.test "column" do
                              let q = select id # from users # wher (exists $ select id # from users)
                              TM.notParameterized """SELECT "id" FROM users WHERE EXISTS (SELECT "id" FROM users)""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 1 }, { id: 2 } ]
                        TU.suite "path" do
                              TU.test "outer" do
                                    let q = select id # from (users # as u) # wher (exists $ select (u ... id) # from users)
                                    TM.notParameterized """SELECT "id" FROM users AS "u" WHERE EXISTS (SELECT "u"."id" "u.id" FROM users)""" $ DLIQ.buildQuery q
                                    TM.result q [ { id: 1 }, { id: 2 } ]
                              TU.test "outer where" do
                                    let q = select id # from (users # as u) # wher (exists $ select (3 # as t) # from users # wher (id .=. u ... id))
                                    TM.notParameterized """SELECT "id" FROM users AS "u" WHERE EXISTS (SELECT 3 AS "t" FROM users WHERE "id" = "u"."id")""" $ DLIQ.buildQuery q
                                    TM.result q [ { id: 1 }, { id: 2 } ]
                              TU.test "outer inner where" do
                                    let q = select id # from (users # as u) # wher (exists $ select id # from (users # as t) # wher (t ... id .=. u ... id))
                                    TM.notParameterized """SELECT "id" FROM users AS "u" WHERE EXISTS (SELECT "id" FROM users AS "t" WHERE "t"."id" = "u"."id")""" $ DLIQ.buildQuery q
                                    TM.result q [ { id: 1 }, { id: 2 } ]

                  TU.suite "mixed" do
                        TU.test "not bracketed" do
                              let q = select id # from users # wher (id .=. 333 .||. id .=. 33 .&&. id .=. 3)
                              TM.parameterized """SELECT "id" FROM users WHERE ("id" = $1 OR ("id" = $2 AND "id" = $3))""" $ DLIQ.buildQuery q
                              TM.result q []
                        TU.test "bracketed" do
                              let q = select id # from users # wher ((id .=. 2 .||. id .=. 22) .&&. id .=. 2)
                              TM.parameterized """SELECT "id" FROM users WHERE (("id" = $1 OR "id" = $2) AND "id" = $3)""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 2 } ]
                        TU.test "with exists" do
                              let q = select id # from users # wher ((id .=. 2 .||. (exists $ select id # from users)) .&&. id .=. 2)
                              TM.parameterized """SELECT "id" FROM users WHERE (("id" = $1 OR EXISTS (SELECT "id" FROM users)) AND "id" = $2)""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 2 } ]
                        TU.test "with not" do
                              let q = select id # from users # wher ((id .=. 2 .||. not (exists $ select id # from users)) .&&. not (id .=. 2))
                              TM.parameterized """SELECT "id" FROM users WHERE (("id" = $1 OR NOT EXISTS (SELECT "id" FROM users)) AND NOT "id" = $2)""" $ DLIQ.buildQuery q
                              TM.result q []

            TU.suite "subqueries" do
                  TU.test "scalar" do
                        let namep = "mary"
                        let q = select (select (4 # as n) # from users # wher (name .=. namep) # as b)
                        TM.parameterized """SELECT (SELECT 4 AS "n" FROM users WHERE "name" = $1) AS "b"""" $ DLIQ.buildQuery q
                        TM.result q [ { b: Just 4 } ]
                  TU.test "field" do
                        let namep = "josh"
                        let q = select (select id # from users # wher (name .=. namep) # as b)
                        TM.parameterized """SELECT (SELECT "id" FROM users WHERE "name" = $1) AS "b"""" $ DLIQ.buildQuery q
                        TM.result q [ { b: Just 1 } ]
                  TU.test "tuple" do
                        let parameters = { d: "mary", e: 2 }
                        let q = select ((3 # as (Proxy ∷ Proxy "e")) /\ (select id # from users # wher (name .=. parameters.d) # as b) /\ (select id # from messages # wher (id .=. parameters.e) # as n))
                        TM.parameterized """SELECT 3 AS "e", (SELECT "id" FROM users WHERE "name" = $1) AS "b", (SELECT "id" FROM messages WHERE "id" = $2) AS "n"""" $ DLIQ.buildQuery q
                        TM.result q [ { e: 3, b: Just 2, n: Just 2 } ]
                  TU.test "where" do
                        let parameters = { d: "mary", e: 2 }
                        let q = select ((3 # as (Proxy ∷ Proxy "e")) /\ (select id # from users # wher (name .=. parameters.d) # as b) /\ (select id # from messages # wher (id .=. parameters.e) # as n)) # from users # wher (id .=. 1 .||. id .=. 2)
                        TM.parameterized """SELECT 3 AS "e", (SELECT "id" FROM users WHERE "name" = $1) AS "b", (SELECT "id" FROM messages WHERE "id" = $2) AS "n" FROM users WHERE ("id" = $3 OR "id" = $4)""" $ DLIQ.buildQuery q
                        TM.result q [ { e: 3, b: Just 2, n: Just 2 }, { e: 3, b: Just 2, n: Just 2 } ]

            TU.suite "references" do
                  TU.test "named table" do
                        let q = select recipient # from (messages # as u) # wher (sender .=. 1)
                        TM.parameterized """SELECT "recipient" FROM messages AS "u" WHERE "sender" = $1""" $ DLIQ.buildQuery q
                        TM.result q [ { recipient: 2 } ]
                  TU.test "named table alias" do
                        let q = select (u ... id) # from (users # as u) # wher (u ... id .<>. 3)
                        TM.parameterized """SELECT "u"."id" "u.id" FROM users AS "u" WHERE "u"."id" <> $1""" $ DLIQ.buildQuery q
                        TM.result q [ { "u.id": 1 }, { "u.id": 2 } ]
                  TU.test "named subquery" do
                        let q = select n # from (select (id # as n) # from users # as u) # wher (u ... n .<>. 3)
                        TM.parameterized """SELECT "n" FROM (SELECT "id" AS "n" FROM users) AS "u" WHERE "u"."n" <> $1""" $ DLIQ.buildQuery q
                        TM.result q [ { n: 1 }, { n: 2 } ]
                  TU.suite "column subquery" do
                        TU.test "outer" do
                              let q = select (select id # from users # wher (u ... id .<>. id)) # from (users # as u)
                              TM.notParameterized """SELECT (SELECT "id" FROM users WHERE "u"."id" <> "id") FROM users AS "u"""" $ DLIQ.buildQuery q
                              TM.result q [ { id: Just 2 }, { id: Just 1 } ]
                        TU.test "value" do
                              let q = select (id /\ (select ((u ... id) # as n) # from users # wher (id .=. 2))) # from (users # as u)
                              TM.parameterized """SELECT "id", (SELECT "u"."id" AS "n" FROM users WHERE "id" = $1) FROM users AS "u"""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 1, n: Just 1 }, { id: 2, n: Just 2 } ]
                        TU.test "inner alias" do
                              let q = select (select (n ... name) # from (users # as n) # wher (u ... id .<>. n ... id)) # from (users # as u)
                              TM.notParameterized """SELECT (SELECT "n"."name" "n.name" FROM users AS "n" WHERE "u"."id" <> "n"."id") FROM users AS "u"""" $ DLIQ.buildQuery q
                              TM.result q [ { "n.name": Just "mary" }, { "n.name": Just "josh" } ]
                        TU.test "same alias" do
                              let q = select (id /\ (select (u ... id) # from (users # as u) # wher (u ... id .<>. u ... id))) # from (users # as u)
                              TM.notParameterized """SELECT "id", (SELECT "u"."id" "u.id" FROM users AS "u" WHERE "u"."id" <> "u"."id") FROM users AS "u"""" $ DLIQ.buildQuery q
                              TM.result q [ { id: 1, "u.id": Nothing }, { id: 2, "u.id": Nothing } ]
                        TU.test "join" do
                              let q = select ((select name # from users # wher (id .=. u ... _by)) # as b) # from (join (tags # as u) (messages # as b) # on (u ... id .=. b ... id)) # wher (u ... id .=. 34)
                              TM.parameterized """SELECT (SELECT "name" FROM users WHERE "id" = "u"."by") AS "b" FROM tags AS "u" INNER JOIN messages AS "b" ON "u"."id" = "b"."id" WHERE "u"."id" = $1""" $ DLIQ.buildQuery q
                              TM.result q []
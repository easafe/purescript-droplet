module Test.Join where

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Language (Table(..), as, from, join, leftJoin, limit, on, orderBy, qualified, select, wher, (.&&.), (...), (.=.))
import Droplet.Language.Internal.Translate as DLIQ
import Prelude
import Test.Model as TM
import Test.RunTestFailure as TR
import Test.Spec (Spec)
import Test.Spec as TS
import Test.Types (Users, b, id, messages, n, name, sender, sent, t, tags, u, users)

tests ∷ Spec Unit
tests = do
      TS.describe "join" do
            TS.describe "invariants" do
                  TS.it "cannot join source to itself" do
                        TR.testFailure "join-test-1" "NoInstanceFound" "JOIN sources must have unique names"
                  TS.it "qualified columns must reference a source alias" do
                        TR.testFailure "join-test-2" "TypesDoNotUnify" "\"u.id\""
                  TS.it "columns on both sides must be qualified" do
                        TR.testFailure "join-test-3" "NoInstanceFound" "Column id is not in scope"
                  TS.it "joining a table to itself requires aliases" do
                        TR.testFailure "join-test-4" "NoInstanceFound" "tables joined more than once must be aliased"
            TS.describe "unaliased tables" do
                  TS.it "columns can be qualified by table name" do
                        let usr = qualified @"users" users
                        let msg = qualified @"messages" messages
                        let q = select (usr.name /\ msg.id) # from (users `join` messages # on (msg.sender .=. usr.id)) # orderBy msg.id
                        TM.notParameterized """SELECT "users"."name" "users.name", "messages"."id" "messages.id" FROM "users" INNER JOIN "messages" ON "messages"."sender" = "users"."id" ORDER BY "messages"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { "users.name": "josh", "messages.id": 1 }, { "users.name": "mary", "messages.id": 2 } ]
                  TS.it "where qualified by table name" do
                        let usr = qualified @"users" users
                        let q = select name # from users # wher (usr.id .=. 2)
                        TM.parameterized """SELECT "name" FROM "users" WHERE "users"."id" = $1""" $ DLIQ.buildQuery q
                        TM.result q [ { name: "mary" } ]
                  TS.it "different tables with the same columns" do
                        let copy = Table ∷ Table "users_copy" Users
                        let usr = qualified @"users" users
                        let cpy = qualified @"users_copy" copy
                        let q = select (usr.name /\ cpy.name) # from (users `join` copy # on (usr.id .=. cpy.id))
                        TM.notParameterized """SELECT "users"."name" "users.name", "users_copy"."name" "users_copy.name" FROM "users" INNER JOIN "users_copy" ON "users"."id" = "users_copy"."id"""" $ DLIQ.buildQuery q
            TS.describe "inner" do
                  TS.it "path column" do
                        let q = select (u ... id /\ sender) # from ((users # as @"u") `join` (messages # as @"t") # on (u ... id .=. t ... id))
                        TM.notParameterized """SELECT "u"."id" "u.id", "sender" FROM "users" AS "u" INNER JOIN "messages" AS "t" ON "u"."id" = "t"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { "u.id": 1, sender: 1 }, { "u.id": 2, sender: 2 } ]
                  TS.it "unique columns" do
                        let q = select (sent /\ name) # from ((users # as @"u") `join` (messages # as @"t") # on (u ... id .=. t ... id))
                        TM.notParameterized """SELECT "sent", "name" FROM "users" AS "u" INNER JOIN "messages" AS "t" ON "u"."id" = "t"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { sent: true, name: "josh" }, { sent: true, name: "mary" } ]
                  TS.it "aliased columns" do
                        let q = select (u ... id # as @"id") # from ((users # as @"u") `join` (messages # as @"t") # on (u ... id .=. t ... id))
                        TM.notParameterized """SELECT "u"."id" AS "id" FROM "users" AS "u" INNER JOIN "messages" AS "t" ON "u"."id" = "t"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 1 }, { id: 2 } ]
                  TS.it "subquery with path column" do
                        let q = select (select (u ... id) # from users # orderBy id # limit @1) # from ((users # as @"u") `join` (messages # as @"t") # on (u ... id .=. t ... id))
                        TM.notParameterized """SELECT (SELECT "u"."id" "u.id" FROM "users" ORDER BY "id" LIMIT 1) FROM "users" AS "u" INNER JOIN "messages" AS "t" ON "u"."id" = "t"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { "u.id": Just 1 }, { "u.id": Just 2 } ]
                  TS.it "subquery with where path column" do
                        let q = select (select id # from users # wher (id .=. u ... id) # orderBy id # limit @1) # from ((users # as @"u") `join` (messages # as @"t") # on (u ... id .=. t ... id))
                        TM.notParameterized """SELECT (SELECT "id" FROM "users" WHERE "id" = "u"."id" ORDER BY "id" LIMIT 1) FROM "users" AS "u" INNER JOIN "messages" AS "t" ON "u"."id" = "t"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: Just 1 }, { id: Just 2 } ]
                  TS.it "aliased subquery with where path column" do
                        let q = select (select id # from (users # as @"b") # wher (b ... id .=. u ... id) # orderBy id # limit @1) # from ((users # as @"u") `join` (messages # as @"t") # on (u ... id .=. t ... id))
                        TM.notParameterized """SELECT (SELECT "id" FROM "users" AS "b" WHERE "b"."id" = "u"."id" ORDER BY "id" LIMIT 1) FROM "users" AS "u" INNER JOIN "messages" AS "t" ON "u"."id" = "t"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: Just 1 }, { id: Just 2 } ]
                  TS.it "three joins" do
                        let q = select (u ... id /\ t ... id /\ b ... id) # from (((users # as @"u") `join` (messages # as @"t") # on (u ... id .=. t ... id)) `join` (tags # as @"b") # on (b ... id .=. u ... id))
                        TM.notParameterized """SELECT "u"."id" "u.id", "t"."id" "t.id", "b"."id" "b.id" FROM "users" AS "u" INNER JOIN "messages" AS "t" ON "u"."id" = "t"."id" INNER JOIN "tags" AS "b" ON "b"."id" = "u"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { "b.id": 1, "t.id": 1, "u.id": 1 } ]
                  TS.it "four joins" do
                        let q = select (u ... id /\ t ... id /\ b ... id /\ n ... id) # from ((((users # as @"u") `join` (messages # as @"t") # on (u ... id .=. t ... id)) `join` (tags # as @"b") # on (b ... id .=. u ... id)) `join` (users # as @"n") # on (n ... id .=. t ... id .&&. n ... id .=. u ... id))
                        TM.notParameterized """SELECT "u"."id" "u.id", "t"."id" "t.id", "b"."id" "b.id", "n"."id" "n.id" FROM "users" AS "u" INNER JOIN "messages" AS "t" ON "u"."id" = "t"."id" INNER JOIN "tags" AS "b" ON "b"."id" = "u"."id" INNER JOIN "users" AS "n" ON ("n"."id" = "t"."id" AND "n"."id" = "u"."id")""" $ DLIQ.buildQuery q
                        TM.result q [ { "b.id": 1, "t.id": 1, "u.id": 1, "n.id": 1 } ]
                  TS.it "subselect outer reference" do
                        let q = select (select name # from (join (users # as @"n") (messages # as @"b") # on (b ... id .=. n ... id .&&. b ... id .=. u ... id)) # orderBy name # limit @1) # from (users # as @"u")
                        TM.notParameterized """SELECT (SELECT "name" FROM "users" AS "n" INNER JOIN "messages" AS "b" ON ("b"."id" = "n"."id" AND "b"."id" = "u"."id") ORDER BY "name" LIMIT 1) FROM "users" AS "u"""" $ DLIQ.buildQuery q
                        TM.result q [ { name: Just "josh" }, { name: Just "mary" } ]

            TS.describe "(left) outer" do
                  TS.it "path column" do
                        let q = select (u ... id /\ sender) # from ((users # as @"u") `leftJoin` (messages # as @"t") # on (u ... id .=. t ... id))
                        TM.notParameterized """SELECT "u"."id" "u.id", "sender" FROM "users" AS "u" LEFT JOIN "messages" AS "t" ON "u"."id" = "t"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { "u.id": 1, sender: Just 1 }, { "u.id": 2, sender: Just 2 } ]
                  TS.it "unique columns" do
                        let q = select (sent /\ name) # from ((users # as @"u") `leftJoin` (messages # as @"t") # on (u ... id .=. t ... id))
                        TM.notParameterized """SELECT "sent", "name" FROM "users" AS "u" LEFT JOIN "messages" AS "t" ON "u"."id" = "t"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { sent: Just true, name: "josh" }, { sent: Just true, name: "mary" } ]
                  TS.it "aliased columns" do
                        let q = select (u ... id # as @"id") # from ((users # as @"u") `leftJoin` (messages # as @"t") # on (u ... id .=. t ... id))
                        TM.notParameterized """SELECT "u"."id" AS "id" FROM "users" AS "u" LEFT JOIN "messages" AS "t" ON "u"."id" = "t"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 1 }, { id: 2 } ]
                  TS.it "subquery with path column" do
                        let q = select (select (u ... id) # from users # orderBy id # limit @1) # from ((users # as @"u") `leftJoin` (messages # as @"t") # on (u ... id .=. t ... id))
                        TM.notParameterized """SELECT (SELECT "u"."id" "u.id" FROM "users" ORDER BY "id" LIMIT 1) FROM "users" AS "u" LEFT JOIN "messages" AS "t" ON "u"."id" = "t"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { "u.id": Just 1 }, { "u.id": Just 2 } ]
                  TS.it "subquery with where path column" do
                        let q = select (select id # from users # wher (id .=. u ... id) # orderBy id # limit @1) # from ((users # as @"u") `leftJoin` (messages # as @"t") # on (u ... id .=. t ... id))
                        TM.notParameterized """SELECT (SELECT "id" FROM "users" WHERE "id" = "u"."id" ORDER BY "id" LIMIT 1) FROM "users" AS "u" LEFT JOIN "messages" AS "t" ON "u"."id" = "t"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: Just 1 }, { id: Just 2 } ]
                  TS.it "aliased subquery with where path column" do
                        let q = select (select id # from (users # as @"b") # wher (b ... id .=. u ... id) # orderBy id # limit @1) # from ((users # as @"u") `leftJoin` (messages # as @"t") # on (u ... id .=. t ... id))
                        TM.notParameterized """SELECT (SELECT "id" FROM "users" AS "b" WHERE "b"."id" = "u"."id" ORDER BY "id" LIMIT 1) FROM "users" AS "u" LEFT JOIN "messages" AS "t" ON "u"."id" = "t"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: Just 1 }, { id: Just 2 } ]
                  TS.it "three leftjoins" do
                        let q = select (u ... id /\ t ... id /\ b ... id) # from (((users # as @"u") `leftJoin` (messages # as @"t") # on (u ... id .=. t ... id)) `leftJoin` (tags # as @"b") # on (b ... id .=. u ... id))
                        TM.notParameterized """SELECT "u"."id" "u.id", "t"."id" "t.id", "b"."id" "b.id" FROM "users" AS "u" LEFT JOIN "messages" AS "t" ON "u"."id" = "t"."id" LEFT JOIN "tags" AS "b" ON "b"."id" = "u"."id"""" $ DLIQ.buildQuery q
                        TM.result q [ { "b.id": Just 1, "t.id": Just 1, "u.id": 1 }, { "b.id": Nothing, "t.id": Just 2, "u.id": 2 } ]
                  TS.it "four leftjoins" do
                        let q = select (u ... id /\ t ... id /\ b ... id /\ n ... id) # from ((((users # as @"u") `leftJoin` (messages # as @"t") # on (u ... id .=. t ... id)) `leftJoin` (tags # as @"b") # on (b ... id .=. u ... id)) `leftJoin` (users # as @"n") # on (n ... id .=. t ... id .&&. n ... id .=. u ... id))
                        TM.notParameterized """SELECT "u"."id" "u.id", "t"."id" "t.id", "b"."id" "b.id", "n"."id" "n.id" FROM "users" AS "u" LEFT JOIN "messages" AS "t" ON "u"."id" = "t"."id" LEFT JOIN "tags" AS "b" ON "b"."id" = "u"."id" LEFT JOIN "users" AS "n" ON ("n"."id" = "t"."id" AND "n"."id" = "u"."id")""" $ DLIQ.buildQuery q
                        TM.result q [ { "b.id": Just 1, "t.id": Just 1, "u.id": 1, "n.id": Just 1 }, { "b.id": Nothing, "t.id": Just 2, "u.id": 2, "n.id": Just 2 } ]
                  TS.it "subselect outer reference" do
                        let q = select (select name # from (leftJoin (users # as @"n") (messages # as @"b") # on (b ... id .=. n ... id .&&. b ... id .=. u ... id)) # orderBy name # limit @1) # from (users # as @"u")
                        TM.notParameterized """SELECT (SELECT "name" FROM "users" AS "n" LEFT JOIN "messages" AS "b" ON ("b"."id" = "n"."id" AND "b"."id" = "u"."id") ORDER BY "name" LIMIT 1) FROM "users" AS "u"""" $ DLIQ.buildQuery q
                        TM.result q [ { name: Just "josh" }, { name: Just "josh" } ]

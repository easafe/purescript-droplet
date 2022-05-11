module Test.SubQuery where

import Droplet.Language
import Prelude
import Test.Types

import Data.Maybe (Maybe(..))
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM
import Type.Proxy(Proxy(..))
import Test.Spec (Spec)
import Test.Spec as TS


tests ∷ Spec Unit
tests = do
      TS.describe "subquery" do
            TS.it "null" do
                  let q = select (select id # from users # wher (id .=. 9999) # orderBy id # limit (Proxy :: _ 1)) # from messages # orderBy id # limit (Proxy :: _ 1)
                  TM.parameterized """SELECT (SELECT "id" FROM "users" WHERE "id" = $1 ORDER BY "id" LIMIT 1) FROM "messages" ORDER BY "id" LIMIT 1""" $ DLIQ.buildQuery q
                  TM.result q [ { id: Nothing } ]
            TS.it "nested null" do
                  let q = select (select created # from tags # orderBy id # limit (Proxy :: _ 1)) # from messages # orderBy id # limit (Proxy :: _ 1)
                  TM.notParameterized """SELECT (SELECT "created" FROM "tags" ORDER BY "id" LIMIT 1) FROM "messages" ORDER BY "id" LIMIT 1""" $ DLIQ.buildQuery q
                  --avoid (Maybe (Maybe t))
                  TM.result q [ { created: Nothing } ]
            TS.it "function" do
                  let q = select (select (coalesce id # as id) # from users # orderBy id # limit (Proxy :: _ 1)) # from messages
                  TM.notParameterized """SELECT (SELECT coalesce("id") AS "id" FROM "users" ORDER BY "id" LIMIT 1) FROM "messages"""" $ DLIQ.buildQuery q
                  TM.result q [ { id: Just 1 }, { id: Just 1 } ]
            TS.describe "outer references" do
                  TS.describe "projection from table" do
                        TS.it "field" do
                              let q = select (select (u ... id) # from users # orderBy id # limit (Proxy :: _ 1)) # from (users # as u) # orderBy id # limit (Proxy :: _ 1)
                              TM.notParameterized """SELECT (SELECT "u"."id" "u.id" FROM "users" ORDER BY "id" LIMIT 1) FROM "users" AS "u" ORDER BY "id" LIMIT 1""" $ DLIQ.buildQuery q
                              TM.result q [ { "u.id": Just 1 } ]
                        TS.it "alias" do
                              let q = select (select (u ... id # as n) # from users # orderBy id # limit (Proxy :: _ 1)) # from (users # as u) # orderBy id # limit (Proxy :: _ 1)
                              TM.notParameterized """SELECT (SELECT "u"."id" AS "n" FROM "users" ORDER BY "id" LIMIT 1) FROM "users" AS "u" ORDER BY "id" LIMIT 1""" $ DLIQ.buildQuery q
                              TM.result q [ { n: Just 1 } ]
                        TS.it "same table different alias" do
                              let q = select (select (n ... name) # from (users # as n) # orderBy id # limit (Proxy :: _ 1)) # from (users # as u)
                              TM.notParameterized """SELECT (SELECT "n"."name" "n.name" FROM "users" AS "n" ORDER BY "id" LIMIT 1) FROM "users" AS "u"""" $ DLIQ.buildQuery q
                              TM.result q [ { "n.name": Just "josh" }, { "n.name": Just "josh" } ]
                        TS.it "same table alias" do
                              let q = select (select (u ... sent) # from (messages # as u) # orderBy id # limit (Proxy :: _ 1)) # from (users # as u)
                              TM.notParameterized """SELECT (SELECT "u"."sent" "u.sent" FROM "messages" AS "u" ORDER BY "id" LIMIT 1) FROM "users" AS "u"""" $ DLIQ.buildQuery q
                              TM.result q [ { "u.sent": Just true }, { "u.sent": Just true } ]
                  TS.describe "projection from named query" do
                        TS.it "field" do
                              let q = select (select (u ... id) # from users # orderBy id # limit (Proxy :: _ 1)) # from (select id # from users # as u)
                              TM.notParameterized """SELECT (SELECT "u"."id" "u.id" FROM "users" ORDER BY "id" LIMIT 1) FROM (SELECT "id" FROM "users") AS "u"""" $ DLIQ.buildQuery q
                              TM.result q [ { "u.id": Just 1 }, { "u.id": Just 2 } ]
                        TS.it "alias" do
                              let q = select (select (u ... id # as n) # from users # orderBy id # limit (Proxy :: _ 1)) # from (select id # from users # as u)
                              TM.notParameterized """SELECT (SELECT "u"."id" AS "n" FROM "users" ORDER BY "id" LIMIT 1) FROM (SELECT "id" FROM "users") AS "u"""" $ DLIQ.buildQuery q
                              TM.result q [ { n: Just 1 }, { n: Just 2 } ]
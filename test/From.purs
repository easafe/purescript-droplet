module Test.From where

import Droplet.Language
import Prelude hiding (join)
import Test.Types

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM

import Test.Spec (Spec)
import Test.Spec as TS
import Type.Proxy (Proxy(..))


tests ∷ Spec Unit
tests = do
      TS.describe "from" do
            TS.it "star" do
                  let q = select star # from messages
                  TM.notParameterized """SELECT * FROM "messages"""" $ DLIQ.buildQuery q
                  void $ TM.resultOnly q
            TS.it "null fields" do
                  let q = select (created /\ _by) # from tags
                  TM.notParameterized """SELECT "created", "by" FROM "tags"""" $ DLIQ.buildQuery q
                  TM.result q [ { created: Nothing, by: Just 1 } ]
            TS.describe "named table" do
                  TS.it "path" do
                        let q = select (u ... id) # from (users # as u)
                        TM.notParameterized """SELECT "u"."id" "u.id" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                        TM.result q [ { "u.id": 1 }, { "u.id": 2 } ]
                  TS.it "aliased path" do
                        let q = select (u ... id # as id) # from (users # as u)
                        TM.notParameterized """SELECT "u"."id" AS "id" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 1 }, { id: 2 } ]
            TS.describe "named queries" do
                  TS.it "star" do
                        let q = select star # from (select (4 # as n) # from messages # as n)
                        TM.notParameterized """SELECT * FROM (SELECT 4 AS "n" FROM "messages") AS "n"""" $ DLIQ.buildQuery q
                        TM.result q [ { n: 4 }, { n: 4 } ]
                  TS.it "star qualified qualified field" do
                        let q = select star # from (select (bigB ... birthday) # from (users # as bigB) # as t)
                        TM.notParameterized """SELECT * FROM (SELECT "B"."birthday" "B.birthday" FROM "users" AS "B") AS "t"""" $ DLIQ.buildQuery q
                        TM.result q [ { "B.birthday": TM.makeDate 1990 1 1 }, { "B.birthday": TM.makeDate 1900 11 11 } ]
                  TS.it "star bracket" do
                        let q = select star # from (select (select id # from users # orderBy id # limit (Proxy :: _ 1)) # from users # as n)
                        TM.notParameterized """SELECT * FROM (SELECT (SELECT "id" FROM "users" ORDER BY "id" LIMIT 1) FROM "users") AS "n"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: Just 1 }, { id: Just 1 } ]
                  TS.it "field" do
                        let q = select birthday # from (select birthday # from users # as t)
                        TM.notParameterized """SELECT "birthday" FROM (SELECT "birthday" FROM "users") AS "t"""" $ DLIQ.buildQuery q
                        TM.result q [ { birthday: TM.makeDate 1990 1 1 }, { birthday: TM.makeDate 1900 11 11 } ]
                  TS.it "qualified field" do
                        let q = select (t ... birthday) # from (select birthday # from users # as t)
                        TM.notParameterized """SELECT "t"."birthday" "t.birthday" FROM (SELECT "birthday" FROM "users") AS "t"""" $ DLIQ.buildQuery q
                        TM.result q [ { "t.birthday": TM.makeDate 1990 1 1 }, { "t.birthday": TM.makeDate 1900 11 11 } ]
                  TS.it "qualified qualified field" do
                        let q = select (t ... (Proxy ∷ Proxy "B.birthday")) # from (select (bigB ... birthday) # from (users # as bigB) # as t)
                        TM.notParameterized """SELECT "t"."B.birthday" "t.B.birthday" FROM (SELECT "B"."birthday" "B.birthday" FROM "users" AS "B") AS "t"""" $ DLIQ.buildQuery q
                        TM.result q [ { "t.B.birthday": TM.makeDate 1990 1 1 }, { "t.B.birthday": TM.makeDate 1900 11 11 } ]
                  TS.it "renamed field" do
                        let q = select t # from (select (birthday # as t) # from users # as t)
                        TM.notParameterized """SELECT "t" FROM (SELECT "birthday" AS "t" FROM "users") AS "t"""" $ DLIQ.buildQuery q
                        TM.result q [ { t: TM.makeDate 1990 1 1 }, { t: TM.makeDate 1900 11 11 } ]
                  TS.it "join" do
                        let q = select star # from (select (u ... name /\ t ... id) # from (join (users # as u) (messages # as t) # on (u ... id .=. t ... id)) # as b)
                        TM.notParameterized """SELECT * FROM (SELECT "u"."name" "u.name", "t"."id" "t.id" FROM "users" AS "u" INNER JOIN "messages" AS "t" ON "u"."id" = "t"."id") AS "b"""" $ DLIQ.buildQuery q
                        TM.result q [ { "t.id": 1, "u.name": "josh" }, { "t.id": 2, "u.name": "mary" } ]

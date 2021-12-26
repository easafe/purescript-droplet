module Test.Union where

import Droplet.Language (as, from, select, union, unionAll, wher, (.<>.), (.=.))
import Prelude
import Test.Types (b, id, messages, name, users)

import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM

import Test.Spec (Spec)
import Test.Spec as TS


tests ∷ Spec Unit
tests = do
      TS.describe "union" do
            TS.it "select" do
                  let q = (select id # from users # wher (name .=. "mary")) `union` (select id # from users # wher (name .<>. "mary"))
                  TM.parameterized """(SELECT "id" FROM "users" WHERE "name" = $1 UNION SELECT "id" FROM "users" WHERE "name" <> $2)""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 2 }, { id: 1 } ]
            TS.it "as" do
                  let q = (select id # from (users # as b)) `union` (select id # from users # wher (name .<>. "mary"))
                  TM.parameterized """(SELECT "id" FROM "users" AS "b" UNION SELECT "id" FROM "users" WHERE "name" <> $1)""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 2 }, { id: 1 } ]
            TS.describe "from union" do
                  TS.it "left" do
                        let q = ((select id # from users) `union` (select id # from messages)) `union` (select id # from users)
                        TM.notParameterized """((SELECT "id" FROM "users" UNION SELECT "id" FROM "messages") UNION SELECT "id" FROM "users")""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 2 }, { id: 1 } ]
                  TS.it "right" do
                        let q = (select id # from users) `union` ((select id # from messages) `union` (select id # from users))
                        TM.notParameterized """(SELECT "id" FROM "users" UNION (SELECT "id" FROM "messages" UNION SELECT "id" FROM "users"))""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 2 }, { id: 1 } ]

      TS.describe "union all" do
            TS.it "select" do
                  let q = (select id # from users # wher (name .=. "mary")) `unionAll` (select id # from users # wher (name .<>. "mary"))
                  TM.parameterized """(SELECT "id" FROM "users" WHERE "name" = $1 UNION ALL SELECT "id" FROM "users" WHERE "name" <> $2)""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 2 }, { id: 1 } ]
            TS.it "as" do
                  let q = (select id # from (users # as b)) `unionAll` (select id # from users # wher (name .<>. "mary"))
                  TM.parameterized """(SELECT "id" FROM "users" AS "b" UNION ALL SELECT "id" FROM "users" WHERE "name" <> $1)""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 1 }, { id: 2 }, { id: 1 } ]
            TS.describe "from unionAll" do
                  TS.it "left" do
                        let q = ((select id # from users) `unionAll` (select id # from messages)) `unionAll` (select id # from users)
                        TM.notParameterized """((SELECT "id" FROM "users" UNION ALL SELECT "id" FROM "messages") UNION ALL SELECT "id" FROM "users")""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 1 }, { id: 2 }, { id: 1 }, { id: 2 }, { id: 1 }, { id: 2 } ]
                  TS.it "right" do
                        let q = (select id # from users) `unionAll` ((select id # from messages) `unionAll` (select id # from users))
                        TM.notParameterized """(SELECT "id" FROM "users" UNION ALL (SELECT "id" FROM "messages" UNION ALL SELECT "id" FROM "users"))""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 1 }, { id: 2 }, { id: 1 }, { id: 2 }, { id: 1 }, { id: 2 } ]

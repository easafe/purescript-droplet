module Test.Union where

import Droplet.Language (as, from, select, union, unionAll, wher, (.<>.), (.=.))
import Prelude
import Test.Types (b, id, messages, name, users)

import Droplet.Language.Internal.Query as DLIQ
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests ∷ TestSuite
tests = do
      TU.suite "union" do
            TU.test "select" do
                  let q = (select id # from users # wher (name .=. "mary")) `union` (select id # from users # wher (name .<>. "mary"))
                  TM.parameterized """(SELECT "id" FROM users WHERE "name" = $1 UNION SELECT "id" FROM users WHERE "name" <> $2)""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 2 }, { id: 1 } ]
            TU.test "as" do
                  let q = (select id # from (users # as b)) `union` (select id # from users # wher (name .<>. "mary"))
                  TM.parameterized """(SELECT "id" FROM users AS "b" UNION SELECT "id" FROM users WHERE "name" <> $1)""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 2 }, { id: 1 } ]
            TU.suite "from union" do
                  TU.test "left" do
                        let q = ((select id # from users) `union` (select id # from messages)) `union` (select id # from users)
                        TM.notParameterized """((SELECT "id" FROM users UNION SELECT "id" FROM messages) UNION SELECT "id" FROM users)""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 2 }, { id: 1 } ]
                  TU.test "right" do
                        let q = (select id # from users) `union` ((select id # from messages) `union` (select id # from users))
                        TM.notParameterized """(SELECT "id" FROM users UNION (SELECT "id" FROM messages UNION SELECT "id" FROM users))""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 2 }, { id: 1 } ]

      TU.suite "union all" do
            TU.test "select" do
                  let q = (select id # from users # wher (name .=. "mary")) `unionAll` (select id # from users # wher (name .<>. "mary"))
                  TM.parameterized """(SELECT "id" FROM users WHERE "name" = $1 UNION ALL SELECT "id" FROM users WHERE "name" <> $2)""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 2 }, { id: 1 } ]
            TU.test "as" do
                  let q = (select id # from (users # as b)) `unionAll` (select id # from users # wher (name .<>. "mary"))
                  TM.parameterized """(SELECT "id" FROM users AS "b" UNION ALL SELECT "id" FROM users WHERE "name" <> $1)""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 1 }, { id: 2 }, { id: 1 } ]
            TU.suite "from unionAll" do
                  TU.test "left" do
                        let q = ((select id # from users) `unionAll` (select id # from messages)) `unionAll` (select id # from users)
                        TM.notParameterized """((SELECT "id" FROM users UNION ALL SELECT "id" FROM messages) UNION ALL SELECT "id" FROM users)""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 1 }, { id: 2 }, { id: 1 }, { id: 2 }, { id: 1 }, { id: 2 } ]
                  TU.test "right" do
                        let q = (select id # from users) `unionAll` ((select id # from messages) `unionAll` (select id # from users))
                        TM.notParameterized """(SELECT "id" FROM users UNION ALL (SELECT "id" FROM messages UNION ALL SELECT "id" FROM users))""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 1 }, { id: 2 }, { id: 1 }, { id: 2 }, { id: 1 }, { id: 2 } ]

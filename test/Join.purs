module Test.Join where

import Droplet.Language
import Prelude hiding (join)
import Test.Types

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests :: TestSuite
tests = do
      TU.suite "join" do
            TU.suite "(right) inner" do
                  TU.test "path column" do
                        let q = select (u ... id /\ t ... sender) # from ((users # as u) `join` (messages # as t) # on (u ... id .=. t ... id))
                        TM.notParameterized "SELECT u.id \"u.id\", t.sender \"t.sender\" FROM users AS \"u\" INNER JOIN messages AS \"t\" ON u.id = t.id" $ Query.query q
                        TM.result q [{"u.id": 1, "t.sender": 1}, {"u.id": 2, "t.sender": 2}]
                  TU.test "unique columns" do
                        let q = select (sent /\ name) # from ((users # as u) `join` (messages # as t) # on (u ... id .=. t ... id))
                        TM.notParameterized "SELECT sent, name FROM users AS \"u\" INNER JOIN messages AS \"t\" ON u.id = t.id" $ Query.query q
                        TM.result q [{sent: true, name: "josh"}, {sent: true, name: "mary"}]
                  TU.test "aliased columns" do
                        let q = select (u ... id # as id) # from ((users # as u) `join` (messages # as t) # on (u ... id .=. t ... id))
                        TM.notParameterized "SELECT u.id AS \"id\" FROM users AS \"u\" INNER JOIN messages AS \"t\" ON u.id = t.id" $ Query.query q
                        TM.result q [{id: 1}, {id: 2}]
                  TU.test "subquery with path column" do
                        let q = select (select (u ... id) # from users # orderBy id # limit 1) # from ((users # as u) `join` (messages # as t) # on (u ... id .=. t ... id))
                        TM.notParameterized "SELECT (SELECT u.id \"u.id\" FROM users ORDER BY id LIMIT 1) FROM users AS \"u\" INNER JOIN messages AS \"t\" ON u.id = t.id" $ Query.query q
                        TM.result q [{"u.id": Just 1}, {"u.id": Just 2}]
                  TU.test "subquery with where path column" do
                        let q = select (select id # from users # wher (id .=. u ... id) ) # from ((users # as u) `join` (messages # as t) # on (u ... id .=. t ... id))
                        TM.notParameterized "SELECT (SELECT id FROM users WHERE id = u.id) FROM users AS \"u\" INNER JOIN messages AS \"t\" ON u.id = t.id" $ Query.query q
                        TM.result q [{id: Just 1}, {id: Just 2}]
                  TU.test "aliased subquery with where path column" do
                        let q = select (select id # from (users # as b) # wher (b ... id .=. u ... id) ) # from ((users # as u) `join` (messages # as t) # on (u ... id .=. t ... id))
                        TM.notParameterized "SELECT (SELECT id FROM users AS \"b\" WHERE b.id = u.id) FROM users AS \"u\" INNER JOIN messages AS \"t\" ON u.id = t.id" $ Query.query q
                        TM.result q [{id: Just 1}, {id: Just 2}]
                  TU.test "three joins" do
                        let q = select (u ... id /\ t ... id /\ b ... id) # from (((users # as u) `join` (messages # as t) # on (u ... id .=. t ... id)) `join` (tags # as b) # on (b ... id .=. u ... id))
                        TM.notParameterized "SELECT u.id \"u.id\", t.id \"t.id\", b.id \"b.id\" FROM users AS \"u\" INNER JOIN messages AS \"t\" ON u.id = t.id INNER JOIN tags AS \"b\" ON b.id = u.id" $ Query.query q
                        TM.result q [{"b.id": 1, "t.id": 1, "u.id": 1 }]
                  TU.test "four joins" do
                        let q = select (u ... id /\ t ... id /\ b ... id /\ n ... id) # from ((((users # as u) `join` (messages # as t) # on (u ... id .=. t ... id)) `join` (tags # as b) # on (b ... id .=. u ... id)) `join` (users # as n) # on (n ... id .=. t ... id .&&. n ... id .=. u ... id))
                        TM.notParameterized "SELECT u.id \"u.id\", t.id \"t.id\", b.id \"b.id\", n.id \"n.id\" FROM users AS \"u\" INNER JOIN messages AS \"t\" ON u.id = t.id INNER JOIN tags AS \"b\" ON b.id = u.id INNER JOIN users AS \"n\" ON (n.id = t.id AND n.id = u.id)" $ Query.query q
                        TM.result q [{"b.id": 1, "t.id": 1, "u.id": 1, "n.id": 1 }]



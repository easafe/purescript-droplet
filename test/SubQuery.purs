module Test.SubQuery where

import Droplet.Language
import Prelude
import Test.Types

import Data.Maybe (Maybe(..))
import Droplet.Language.Internal.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests :: TestSuite
tests = do
      TU.suite "subquery" do
            TU.test "null" do
                  let q = select (select id # from users # wher (id .=. 1)) # from messages # orderBy id # limit 1
                  TM.parameterized """SELECT (SELECT id FROM users WHERE id = $1) FROM messages ORDER BY id LIMIT 1""" $ Query.query q
                  TM.result q [{id: Just 1}]
            TU.test "nested null" do
                  let q = select (select created # from tags) # from messages # orderBy id # limit 1
                  TM.notParameterized """SELECT (SELECT created FROM tags) FROM messages ORDER BY id LIMIT 1""" $ Query.query q
                  --avoid (Maybe (Maybe t))
                  TM.result q [{created: Nothing}]
            TU.suite "outer references" do
                  TU.suite "projection from table" do
                        TU.test "field" do
                              let q = select (select (u ... id) # from users # orderBy id # limit 1) # from (users # as u) # orderBy id # limit 1
                              TM.notParameterized """SELECT (SELECT u.id "u.id" FROM users ORDER BY id LIMIT 1) FROM users AS "u" ORDER BY id LIMIT 1""" $ Query.query q
                              TM.result q [{"u.id": Just 1}]
                        TU.test "alias" do
                              let q = select (select (u ... id # as n) # from users # orderBy id # limit 1) # from (users # as u) # orderBy id # limit 1
                              TM.notParameterized """SELECT (SELECT u.id AS "n" FROM users ORDER BY id LIMIT 1) FROM users AS "u" ORDER BY id LIMIT 1""" $ Query.query q
                              TM.result q [{n: Just 1}]
                        TU.test "same table different alias" do
                              let q = select (select (n ... name) # from (users # as n) # orderBy id # limit 1) # from (users # as u)
                              TM.notParameterized """SELECT (SELECT n.name "n.name" FROM users AS "n" ORDER BY id LIMIT 1) FROM users AS "u"""" $ Query.query q
                              TM.result q [{"n.name": Just "josh"}, {"n.name": Just "josh"}]
                        TU.test "same table alias" do
                              let q = select (select (u ... sent) # from (messages # as u) # orderBy id # limit 1) # from (users # as u)
                              TM.notParameterized """SELECT (SELECT u.sent "u.sent" FROM messages AS "u" ORDER BY id LIMIT 1) FROM users AS "u"""" $ Query.query q
                              TM.result q [{"u.sent": Just true}, {"u.sent": Just true}]
                  TU.suite "projection from named query" do
                        TU.test "field" do
                              let q = select (select (u ... id) # from users # orderBy id # limit 1) # from (select id # from users # as u)
                              TM.notParameterized """SELECT (SELECT u.id "u.id" FROM users ORDER BY id LIMIT 1) FROM (SELECT id FROM users) AS "u"""" $ Query.query q
                              TM.result q [{"u.id": Just 1}, {"u.id": Just 2}]
                        TU.test "alias" do
                              let q = select (select (u ... id # as n) # from users # orderBy id # limit 1) # from (select id # from users # as u)
                              TM.notParameterized """SELECT (SELECT u.id AS "n" FROM users ORDER BY id LIMIT 1) FROM (SELECT id FROM users) AS "u"""" $ Query.query q
                              TM.result q [{n: Just 1}, {n: Just 2}]
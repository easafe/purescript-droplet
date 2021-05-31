module Test.From where

import Droplet.Language
import Prelude
import Test.Types

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests :: TestSuite
tests = do
      TU.suite "from" do
            TU.test "table" do
                  let q = select star # from messages
                  TM.notParameterized "SELECT * FROM messages" $ Query.query q
                  TM.result' q []
            TU.test "null fields" do
                  let q = select (created /\ _by) # from tags
                  TM.notParameterized "SELECT created, by FROM tags" $ Query.query q
                  TM.result q [{created : Nothing, by: Just 1 }]
            TU.test "named table" do
                  let q = select (u ... id) # from (users # as u)
                  TM.notParameterized """SELECT u.id AS "u.id" FROM users AS u""" $ Query.query q
                  TM.result q [{"u.id": 1}, {"u.id": 2}]
            TU.suite "named queries" do
                  TU.test "star" do
                        let q = select star # from (select (4 # as n) # from messages # as n)
                        TM.notParameterized "SELECT * FROM (SELECT 4 AS n FROM messages) AS n" $ Query.query q
                        TM.result q [{n: 4}, {n: 4}]
                  TU.test "field" do
                        let q = select birthday # from (select birthday # from users # as t)
                        TM.notParameterized "SELECT birthday FROM (SELECT birthday FROM users) AS t" $ Query.query q
                        TM.result q [{birthday: TM.makeDate 1990 1 1}, {birthday: TM.makeDate 1900 11 11}]
                  TU.test "renamed field" do
                        let q = select t # from (select (birthday # as t) # from users # as t)
                        TM.notParameterized "SELECT t FROM (SELECT birthday AS t FROM users) AS t" $ Query.query q
                        TM.result q [{t: TM.makeDate 1990 1 1}, {t: TM.makeDate 1900 11 11}]
                  TU.testSkip "sub query" do
                        let q = select date # from (select (select date # from messages) # from users # as t)
                        --needs limit
                        TM.notParameterized "SELECT date FROM (SELECT (SELECT date FROM messages) FROM users) AS t" $ Query.query q
                        TM.result q [{date: Just $ TM.makeDateTime 2000 3 4}, {date: Just $ TM.makeDateTime 2000 3 4}]
                  TU.testSkip "tuple" do
                        let q = select (id /\ date /\ (4 # as n) /\ sent) # from (select (id /\ date /\ (4 # as n) /\ (select sent # from messages)) # from messages # as t)
                        TM.notParameterized "SELECT id, date, 4 AS n, sent FROM (SELECT id, date, 4 AS n, (SELECT sent FROM messages) AS sent FROM messages) AS t" $ Query.query q
                        --needs limit
                        TM.result q [{id: 1, date: TM.makeDateTime 2000 3 4, n: 4, sent: Just true}, {id: 1, date: TM.makeDateTime 2000 3 4, n: 4, sent: Just true}]

module Test.As where

import Droplet.Language
import Prelude
import Test.Types

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU
import Type.Proxy (Proxy(..))

tests :: TestSuite
tests =
      TU.suite "as" do
            TU.suite "named field" do
                  TU.test "casing" do
                        let q = select (id # as (Proxy :: Proxy "AbCd")) # from users
                        TM.notParameterized """SELECT id AS "AbCd" FROM users""" $ Query.query q
                        TM.result q [{"AbCd": 1}, {"AbCd": 2}]
            TU.suite "named table" do
                  TU.test "field" do
                        let q = select id # from (users # as u)
                        TM.notParameterized """SELECT id FROM users AS "u"""" $ Query.query q
                        TM.result q [{id: 1}, {id: 2}]
                  TU.test "named aliased field" do
                        let q = select (u ... id # as id) # from (users # as u)
                        TM.notParameterized """SELECT u.id AS "id" FROM users AS "u"""" $ Query.query q
                        TM.result q [{id : 1}, {id: 2}]
            TU.suite "named queries" do
                  TU.test "scalar" do
                        let q = select (4 # as n) # from (select (4 # as n) # from users # wher (id .=. id) # as u)
                        TM.notParameterized """SELECT 4 AS "n" FROM (SELECT 4 AS "n" FROM users WHERE id = id) AS "u"""" $ Query.query q
                        TM.result q [{n: 4}, {n: 4}]
                  TU.test "field" do
                        let q = select id # from (select id # from messages # wher (id .=. id) # as t)
                        TM.notParameterized """SELECT id FROM (SELECT id FROM messages WHERE id = id) AS "t"""" $ Query.query q
                        TM.result q [{id: 1}, {id: 2}]
                  TU.testSkip "sub query" do
                        let q = select id # from (select (select id # from messages # wher (id .=. id)) # from users # wher (id .=. id) # as t)
                        TM.notParameterized """SELECT id FROM (SELECT (SELECT id FROM messages WHERE id = id) FROM users WHERE id = id) AS "t"""" $ Query.query q
                        --needs limit
                        TM.result q [{id: Just 1}, {id: Just 2}]
                  TU.testSkip "tuple" do
                        let q = select (id /\ date /\ (4 # as n) /\ sent) # from (select (id /\ date /\ (4 # as n) /\ (select sent # from messages # wher (id .=. id))) # from messages # wher (id .=. id) # as t)
                        TM.notParameterized """SELECT id, date, 4 AS "n", "sent" FROM (SELECT id, date, 4 AS "n", (SELECT sent FROM messages WHERE id = id) FROM messages WHERE id = id) AS "t"""" $ Query.query q
                        --needs limit
                        --TM.result q [{id: 1}, {id: 2}]
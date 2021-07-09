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
                        TM.notParameterized """SELECT "u".id AS "id" FROM users AS "u"""" $ Query.query q
                        TM.result q [{id : 1}, {id: 2}]
            TU.suite "named queries" do
                  TU.test "subquery column" do
                        let q = select (select id # from users # wher (name .=. "mary") # as b) # from users # wher (id .=. 1 .||. id .=. 2)
                        TM.parameterized """SELECT (SELECT id FROM users WHERE name = $1) AS "b" FROM users WHERE (id = $2 OR id = $3)""" $ Query.query q
                        TM.result q [{b : Just 2}, {b: Just 2}]
                  TU.test "scalar" do
                        let q = select (4 # as n) # from (select (4 # as n) # from users # wher (id .=. id) # as u)
                        TM.notParameterized """SELECT 4 AS "n" FROM (SELECT 4 AS "n" FROM users WHERE id = id) AS "u"""" $ Query.query q
                        TM.result q [{n: 4}, {n: 4}]
                  TU.test "field" do
                        let q = select id # from (select id # from messages # wher (id .=. id) # as t)
                        TM.notParameterized """SELECT id FROM (SELECT id FROM messages WHERE id = id) AS "t"""" $ Query.query q
                        TM.result q [{id: 1}, {id: 2}]
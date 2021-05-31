module Test.Where where

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
tests = do
      TU.suite "where" do
            TU.suite "compared to parameter" do
                  TU.test "equals" do
                        let q = select recipient # from messages # wher (sender .=. 1)
                        TM.parameterized "SELECT recipient FROM messages WHERE sender = $1" $ Query.query q
                        TM.result q [{recipient :  2}]
                  TU.test "not equals" do
                        let q = select sender # from messages # wher (recipient .<>. 2)
                        TM.parameterized "SELECT sender FROM messages WHERE recipient <> $1" $ Query.query q
                        TM.result q [{sender : 2}]
                  TU.test "lesser than" do
                        let q = select sender # from messages # wher (recipient .<. 2)
                        TM.parameterized "SELECT sender FROM messages WHERE recipient < $1" $ Query.query q
                        TM.result q [{sender : 2}]

            TU.suite "compared to field" do
                  TU.test "equals" do
                        let q = select (34 # as n) # from users # wher (name .=. surname)
                        TM.notParameterized """SELECT 34 AS "n" FROM users WHERE name = surname""" $ Query.query q
                        TM.result q []
                  TU.test "not equals" do
                        let q = select (34 # as n) # from users # wher (name .<>. surname)
                        TM.notParameterized """SELECT 34 AS "n" FROM users WHERE name <> surname""" $ Query.query q
                        TM.result q [{n : 34}, {n: 34}]
                  TU.test "greater than" do
                        let q = select sender # from messages # wher (recipient .>. 2)
                        TM.parameterized "SELECT sender FROM messages WHERE recipient > $1" $ Query.query q
                        TM.result q []

            TU.suite "logical operands" do
                  TU.suite "and" do
                        TU.test "single" do
                              let q = select id # from users # wher (name .=. "josh" .&&. name .<>. surname)
                              TM.parameterized "SELECT id FROM users WHERE (name = $1 AND name <> surname)" $ Query.query q
                              TM.result q [{id: 1}]
                        TU.test "many" do
                              let q = select id # from users # wher (name .=. "josh" .&&. "josh" .=. name .&&. surname .=. "j.")
                              TM.parameterized "SELECT id FROM users WHERE ((name = $1 AND $2 = name) AND surname = $3)" $ Query.query q
                              TM.result q [{id: 1}]

                  TU.suite "or" do
                        TU.test "single" do
                              let q = select id # from users # wher (name .=. "mary" .||. name .=. surname)
                              TM.parameterized "SELECT id FROM users WHERE (name = $1 OR name = surname)" $ Query.query q
                              TM.result q [{id: 2}]
                        TU.test "many" do
                              let q = select id # from users # wher (name .=. "josh" .||. name .=. "j." .||. surname .<>. "josh")
                              TM.parameterized "SELECT id FROM users WHERE ((name = $1 OR name = $2) OR surname <> $3)" $ Query.query q
                              TM.result q [{id: 1}, {id: 2}]

                  TU.suite "mixed" do
                        TU.test "not bracketed" do
                              let q = select id # from users # wher (id .=. 333 .||. id .=. 33 .&&. id .=. 3)
                              TM.parameterized "SELECT id FROM users WHERE (id = $1 OR (id = $2 AND id = $3))" $ Query.query q
                              TM.result q []
                        TU.test "bracketed" do
                              let q = select id # from users # wher ((id .=. 2 .||. id .=. 22) .&&. id .=. 2)
                              TM.parameterized "SELECT id FROM users WHERE ((id = $1 OR id = $2) AND id = $3)" $ Query.query q
                              TM.result q [{id: 2 }]

            TU.suite "subqueries" do
                  TU.test "scalar" do
                        let namep = "mary"
                        let q = select (select (4 # as n) # from users # wher (name .=. namep) # as b)
                        TM.parameterized """SELECT (SELECT 4 AS "n" FROM users WHERE name = $1) AS "b"""" $ Query.query q
                        TM.result q [{b: Just 4 }]
                  TU.test "field" do
                        let namep = "josh"
                        let q = select (select id # from users # wher (name .=. namep) # as b)
                        TM.parameterized """SELECT (SELECT id FROM users WHERE name = $1) AS "b"""" $ Query.query q
                        TM.result q [{b: Just 1 }]
                  TU.test "tuple" do
                        let parameters = { d : "mary", e : 2 }
                        let q = select ((3 # as (Proxy :: Proxy "e")) /\ (select id # from users # wher (name .=. parameters.d) # as b) /\ (select id # from messages # wher (id .=. parameters.e) # as n))
                        TM.parameterized """SELECT 3 AS "e", (SELECT id FROM users WHERE name = $1) AS "b", (SELECT id FROM messages WHERE id = $2) AS "n"""" $ Query.query q
                        TM.result q [{e: 3, b: Just 2, n: Just 2 }]
                  TU.test "where" do
                        let parameters = { d : "mary", e : 2 }
                        let q = select ((3 # as (Proxy :: Proxy "e")) /\ (select id # from users # wher (name .=. parameters.d) # as b) /\ (select id # from messages # wher (id .=. parameters.e) # as n)) # from users # wher (id .=. 1 .||. id .=. 2)
                        TM.parameterized """SELECT 3 AS "e", (SELECT id FROM users WHERE name = $1) AS "b", (SELECT id FROM messages WHERE id = $2) AS "n" FROM users WHERE (id = $3 OR id = $4)""" $ Query.query q
                        TM.result q [{e: 3, b: Just 2, n: Just 2 }, {e: 3, b: Just 2, n: Just 2 }]


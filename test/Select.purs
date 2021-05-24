module Test.Select where

import Droplet.Language
import Prelude
import Test.Types

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Internal.Language.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU
import Type.Proxy (Proxy(..))

--lets clean these test out of these meaningless suite categories
tests :: TestSuite
tests = do
      TU.suite "from" do
            TU.test "table" do
                  let q = select (34 # as b) # from messages
                  TM.notParameterized "SELECT 34 AS b FROM messages" $ Query.query q
                  TM.result q [{b : 34}, {b : 34}]
            TU.test "null fields" do
                  let q = select (created /\ _by) # from tags
                  TM.notParameterized "SELECT created, by FROM tags" $ Query.query q
                  TM.result q [{created : Nothing, by: Just 1 }]

      TU.suite "parameters" do
            TU.test "equals" do
                  let q = select recipient # from messages # wher (sender .=. 1)
                  TM.parameterized "SELECT recipient FROM messages WHERE sender = $1" $ Query.query q
                  TM.result q [{recipient :  2}]
            TU.test "not equals" do
                  let q = select sender # from messages # wher (recipient .<>. 2)
                  TM.parameterized "SELECT sender FROM messages WHERE recipient <> $1" $ Query.query q
                  TM.result q [{sender : 2}]

            TU.suite "field" do
                  TU.test "equals" do
                        let q = select (34 # as n) # from users # wher (name .=. surname)
                        TM.notParameterized "SELECT 34 AS n FROM users WHERE name = surname" $ Query.query q
                        TM.result q []
                  TU.test "not equals" do
                        let q = select (34 # as n) # from users # wher (name .<>. surname)
                        TM.notParameterized "SELECT 34 AS n FROM users WHERE name <> surname" $ Query.query q
                        TM.result q [{n : 34}, {n: 34}]

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

                  TU.suite "tuple" do
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
                        TM.parameterized "SELECT (SELECT 4 AS n FROM users WHERE name = $1) AS b" $ Query.query q
                        TM.result q [{b: 4 }]
                  TU.test "field" do
                        let namep = "josh"
                        let q = select (select id # from users # wher (name .=. namep) # as b)
                        TM.parameterized "SELECT (SELECT id FROM users WHERE name = $1) AS b" $ Query.query q
                        TM.result q [{b: 1 }]
                  TU.test "tuple" do
                        let parameters = { d : "mary", e : 2 }
                        let q = select ((3 # as (Proxy :: Proxy "e")) /\ (select id # from users # wher (name .=. parameters.d) # as b) /\ (select id # from messages # wher (id .=. parameters.e) # as n))
                        TM.parameterized "SELECT 3 AS e, (SELECT id FROM users WHERE name = $1) AS b, (SELECT id FROM messages WHERE id = $2) AS n" $ Query.query q
                        TM.result q [{e: 3, b: 2, n: 2 }]
                  TU.test "where" do
                        let parameters = { d : "mary", e : 2 }
                        let q = select ((3 # as (Proxy :: Proxy "e")) /\ (select id # from users # wher (name .=. parameters.d) # as b) /\ (select id # from messages # wher (id .=. parameters.e) # as n)) # from users # wher (id .=. 1 .||. id .=. 2)
                        TM.parameterized "SELECT 3 AS e, (SELECT id FROM users WHERE name = $1) AS b, (SELECT id FROM messages WHERE id = $2) AS n FROM users WHERE (id = $3 OR id = $4)" $ Query.query q
                        TM.result q [{e: 3, b: 2, n: 2 }, {e: 3, b: 2, n: 2 }]

      TU.suite "as" do
            TU.suite "from" do
                  TU.test "scalar" do
                        let q = select (4 # as n) # from (select (4 # as n) # from messages # as (Proxy :: Proxy "u"))
                        TM.notParameterized "SELECT 4 AS n FROM (SELECT 4 AS n FROM messages) AS u" $ Query.query q
                        TM.result q [{n: 4}, {n: 4}]
                  TU.test "field" do
                        let q = select birthday # from (select birthday # from users # as t)
                        TM.notParameterized "SELECT birthday FROM (SELECT birthday FROM users) AS t" $ Query.query q
                        TM.result q [{birthday: TM.makeDate 1990 1 1}, {birthday: TM.makeDate 1900 11 11}]
                  TU.test "renamed field" do
                        let q = select (Proxy :: Proxy "t") # from (select (birthday # as t) # from users # as t)
                        TM.notParameterized "SELECT t FROM (SELECT birthday AS t FROM users) AS t" $ Query.query q
                        TM.result q [{t: TM.makeDate 1990 1 1}, {t: TM.makeDate 1900 11 11}]
                  TU.testSkip "sub query" do
                        let q = select date # from (select (select date # from messages) # from users # as t)
                        --needs limit
                        TM.notParameterized "SELECT date FROM (SELECT (SELECT date FROM messages) FROM users) AS t" $ Query.query q
                        TM.result q [{date: TM.makeDateTime 2000 3 4}, {date: TM.makeDateTime 2000 3 4}]
                  TU.testSkip "tuple" do
                        let q = select (id /\ date /\ (4 # as n) /\ sent) # from (select (id /\ date /\ (4 # as n) /\ (select sent # from messages)) # from messages # as t)
                        TM.notParameterized "SELECT id, date, 4 AS n, sent FROM (SELECT id, date, 4 AS n, (SELECT sent FROM messages) AS sent FROM messages) AS t" $ Query.query q
                        --needs limit
                        TM.result q [{id: 1, date: TM.makeDateTime 2000 3 4, n: 4, sent: true}, {id: 1, date: TM.makeDateTime 2000 3 4, n: 4, sent: true}]

            TU.suite "where" do
                  TU.test "scalar" do
                        let q = select (4 # as n) # from (select (4 # as n) # from users # wher (id .=. id) # as (Proxy :: Proxy "u"))
                        TM.notParameterized "SELECT 4 AS n FROM (SELECT 4 AS n FROM users WHERE id = id) AS u" $ Query.query q
                        TM.result q [{n: 4}, {n: 4}]
                  TU.test "field" do
                        let q = select id # from (select id # from messages # wher (id .=. id) # as t)
                        TM.notParameterized "SELECT id FROM (SELECT id FROM messages WHERE id = id) AS t" $ Query.query q
                        TM.result q [{id: 1}, {id: 2}]
                  TU.testSkip "sub query" do
                        let q = select (Proxy :: Proxy "id") # from (select (select id # from messages # wher (id .=. id)) # from users # wher (id .=. id) # as t)
                        TM.notParameterized "SELECT id FROM (SELECT (SELECT id FROM messages WHERE id = id) FROM users WHERE id = id) AS t" $ Query.query q
                        --needs limit
                        TM.result q [{id: 1}, {id: 2}]
                  TU.testSkip "tuple" do
                        let q = select (id /\ date /\ (4 # as n) /\ sent) # from (select (id /\ date /\ (4 # as n) /\ (select sent # from messages # wher (id .=. id))) # from messages # wher (id .=. id) # as t)
                        TM.notParameterized "SELECT id, date, 4 AS n, sent FROM (SELECT id, date, 4 AS n, (SELECT sent FROM messages WHERE id = id) FROM messages WHERE id = id) AS t" $ Query.query q
                        --needs limit
                        --TM.result q [{id: 1}, {id: 2}]

            TU.suite "parameters" do
                  TU.testSkip "scalar" do
                        let parameters = {date: TM.makeDate 2000 3 4}
                        let q = select (4 # as n) # from (select (4 # as n) # from users # wher (joined .=. parameters.date) # as (Proxy :: Proxy "u"))
                        TM.parameterized "SELECT 4 AS n FROM (SELECT 4 AS n FROM users WHERE joined = $1) AS u" $ Query.query q
                        TM.result q [{n: 4}, {n: 4}]
                  TU.test "field" do
                        let parameters = {date : TM.makeDateTime 2000 3 4 }
                        let q = select id # from (select id # from messages # wher (date .=. parameters.date) # as t)
                        TM.parameterized "SELECT id FROM (SELECT id FROM messages WHERE date = $1) AS t" $ Query.query q
                        TM.result q [{id: 1}, {id: 2}]
                  TU.testSkip "sub query" do
                        let parameters = {id : 1 }
                        let q = select id # from (select (select id # from messages # wher (parameters.id .=. id)) # from users # wher (id .=. parameters.id) # as t)
                        TM.parameterized "SELECT id FROM (SELECT (SELECT id FROM messages WHERE $1 = id) FROM users WHERE id = $2) AS t" $ Query.query q
                        TM.result q [{id: 1}, {id: 2}]
                  TU.testSkip "tuple" do
                        let parameters = {id : 3 }
                        --needs limit
                        let q = select (id /\ date /\ (4 # as n) /\ sent) # from (select (id /\ date /\ (4 # as n) /\ (select sent # from messages # wher (parameters.id .=. parameters.id))) # from messages # wher (parameters.id .=. id) # as t)
                        TM.parameterized "SELECT id, date, 4 AS n, sent FROM (SELECT id, date, 4 AS n, (SELECT sent FROM messages WHERE $1 = $1) FROM messages WHERE $1 = id) AS t" $ Query.query q
                        --TM.result q [{id: 1}, {id: 2}]

      TU.suite "naked select" do
            TU.test "scalar" do
                  let q = select (3 # as n)
                  TM.notParameterized "SELECT 3 AS n" $ Query.query q
                  TM.result q [{n : 3}]
            TU.testSkip "sub query" do
                  let q = select (select (34 # as n) # from users # wher (name .=. name) # as t)
                  TM.notParameterized "SELECT (SELECT 34 AS n FROM users WHERE name = name) AS t" $ Query.query q
                  TM.result q [{t : 34}, {t: 34}]
            TU.test "tuple" do
                  --subqueries need to have type Maybe
                  let q = select ((3 # as b) /\ (select (34 # as n) # from users # wher (name .=. surname) # as t) /\ (4 # as (Proxy :: Proxy "a")) /\ (select name # from users # as (Proxy :: Proxy "u")))
                  TM.notParameterized "SELECT 3 AS b, (SELECT 34 AS n FROM users WHERE name = surname) AS t, 4 AS a, (SELECT name FROM users) AS u" $ Query.query q
                 -- TM.result q [{b: 3, n: 34, a : 4, name: "mary"}, {b: 3, n: 34, a : 4, name: "mary"}]

module Test.As where

import Droplet.Language
import Prelude
import Test.Types

import Data.Tuple.Nested ((/\))
import Droplet.Internal.Language.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU
import Type.Proxy (Proxy(..))

tests :: TestSuite
tests =
      TU.suite "as" do
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
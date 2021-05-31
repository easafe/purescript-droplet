module Test.Limit where

import Droplet.Language
import Prelude
import Test.Types

import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests :: TestSuite
tests =
      TU.suite "limit" do
            TU.test "from" do
                  let q = select (4 # as n) # from users # orderBy n # limit 4
                  TM.notParameterized """SELECT 4 AS "n" FROM users ORDER BY n LIMIT 4""" $ Query.query q
                  TM.result q [{n: 4}, {n: 4}]
            TU.test "where" do
                  let q = select id # from users # wher (id .<>. 10) # orderBy (id /\ name) # limit 2
                  TM.parameterized """SELECT id FROM users WHERE id <> $1 ORDER BY id, name LIMIT 2""" $ Query.query q
                  TM.result q [{id: 1}, {id : 2}]


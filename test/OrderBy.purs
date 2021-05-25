module Test.OrderBy where

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
      TU.suite "order by" do
            TU.test "projection" do
                  let q = select (4 # as n) # from users # orderBy n
                  TM.notParameterized "SELECT 4 AS n FROM users ORDER BY n" $ Query.query q
                  TM.result q [{n: 4}, {n: 4}]
            TU.test "field name" do
                  let q = select id # from users # wher (id .<>. 10) # orderBy (id /\ name)
                  TM.parameterized "SELECT id FROM users WHERE id <> $1 ORDER BY id, name" $ Query.query q
                  TM.result q [{id: 1}, {id : 2}]
            TU.test "asc" do
                  let q = select (id # as n) # from users # wher (id .<>. 4 ) # orderBy (n /\ (name # asc))
                  TM.parameterized "SELECT id AS n FROM users WHERE id <> $1 ORDER BY n, name ASC" $ Query.query q
                  TM.result q [{n: 1}, {n: 2}]
            TU.test "desc" do
                  let q = select id # from users # orderBy (id # desc)
                  TM.notParameterized "SELECT id FROM users ORDER BY id DESC" $ Query.query q
                  TM.result q [{id: 2}, {id: 1}]


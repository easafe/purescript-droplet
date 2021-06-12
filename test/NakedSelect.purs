module Test.NakedSelect where

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
tests =
      TU.suite "naked select" do
            TU.test "scalar" do
                  let q = select (3 # as n)
                  TM.notParameterized """SELECT 3 AS "n"""" $ Query.query q
                  TM.result q [{n : 3}]
            TU.test "subquery" do
                  let q = select (select (34 # as n) # from users # wher (name .=. name) # orderBy id # limit 1)
                  TM.notParameterized """SELECT (SELECT 34 AS "n" FROM users WHERE name = name ORDER BY id LIMIT 1)""" $ Query.query q
                  TM.result q [{n : Just 34}]
            TU.test "aliases" do
                  let q = select (select (u ... id) # from (users # as u) # orderBy id # limit 1)
                  TM.notParameterized """SELECT (SELECT "u".id "u.id" FROM users AS "u" ORDER BY id LIMIT 1)""" $ Query.query q
                  TM.result q [{"u.id" : Just 1}]
            TU.test "named subquery" do
                  let q = select (select (34 # as n) # from users # wher (name .=. name) # orderBy id # limit 1 # as t)
                  TM.notParameterized """SELECT (SELECT 34 AS "n" FROM users WHERE name = name ORDER BY id LIMIT 1) AS "t"""" $ Query.query q
                  TM.result q [{t : Just 34}]
            TU.test "tuple" do
                  let q = select ((3 # as b) /\ (select name # from users # orderBy name # limit 1) /\ (select (u ... name # as n) # from (users # as u) # orderBy (name # desc) # limit 1))
                  TM.notParameterized """SELECT 3 AS "b", (SELECT name FROM users ORDER BY name LIMIT 1), (SELECT "u".name AS "n" FROM users AS "u" ORDER BY name DESC LIMIT 1)""" $ Query.query q
                  TM.result q [{b: 3, name: Just "josh", n: Just "mary"}]

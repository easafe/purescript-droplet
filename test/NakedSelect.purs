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

tests ∷ TestSuite
tests =
      TU.suite "naked select" do
            TU.test "scalar" do
                  let q = select (3 # as n)
                  TM.notParameterized """SELECT 3 AS "n"""" $ Query.query q
                  TM.result q [ { n: 3 } ]
            TU.suite "function" do
                  TU.test "regular" do
                        let q = select (date_part_age ("year" /\ TM.makeDateTime 2000 1 1) # as u)
                        TM.parameterized """SELECT date_part_age($1, $2) AS "u"""" $ Query.query q
                        void $ TM.resultOnly q
                  TU.test "side effect" do
                        let q = select (fire_missiles (9 /\ 8) # as u)
                        TM.parameterized """SELECT fire_missiles($1, $2) AS "u"""" $ Query.query q
                        TM.result q [ { u: unit } ]
                  TU.test "side effect without parameters" do
                        let q = select (random # as u)
                        TM.notParameterized """SELECT random() AS "u"""" $ Query.query q
                        void $ TM.resultOnly q
            TU.test "subquery" do
                  let q = select (select (34 # as n) # from users # wher (name .=. name) # orderBy id # limit 1)
                  TM.notParameterized """SELECT (SELECT 34 AS "n" FROM users WHERE name = name ORDER BY id LIMIT 1)""" $ Query.query q
                  TM.result q [ { n: Just 34 } ]
            TU.test "aliases" do
                  let q = select (select (u ... id) # from (users # as u) # orderBy id # limit 1)
                  TM.notParameterized """SELECT (SELECT "u"."id" "u.id" FROM users AS "u" ORDER BY id LIMIT 1)""" $ Query.query q
                  TM.result q [ { "u.id": Just 1 } ]
            TU.test "named subquery" do
                  let q = select (select (34 # as n) # from users # wher (name .=. name) # orderBy id # limit 1 # as t)
                  TM.notParameterized """SELECT (SELECT 34 AS "n" FROM users WHERE name = name ORDER BY id LIMIT 1) AS "t"""" $ Query.query q
                  TM.result q [ { t: Just 34 } ]
            TU.test "tuple" do
                  let q = select ((3 # as b) /\ (select name # from users # orderBy name # limit 1) /\ (select (u ... name # as n) # from (users # as u) # orderBy (name # desc) # limit 1))
                  TM.notParameterized """SELECT 3 AS "b", (SELECT name FROM users ORDER BY name LIMIT 1), (SELECT "u"."name" AS "n" FROM users AS "u" ORDER BY name DESC LIMIT 1)""" $ Query.query q
                  TM.result q [ { b: 3, name: Just "josh", n: Just "mary" } ]

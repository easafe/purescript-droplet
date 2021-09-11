module Test.Distinct where

import Droplet.Language
import Prelude
import Test.Types
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests ∷ TestSuite
tests =
      TU.suite "distinct" do
            TU.test "field" do
                  let q = select (distinct id) # from messages
                  TM.notParameterized """SELECT DISTINCT id FROM messages""" $ Query.query q
                  TM.result' q []
            TU.test "fields" do
                  let q = select (distinct (id /\ name)) # from users
                  TM.notParameterized """SELECT DISTINCT id, name FROM users""" $ Query.query q
                  TM.result q [ { id: 1, name: "josh" }, { id: 2, name: "mary" } ]
            TU.test "qualified fields" do
                  let q = select (distinct (u ... id /\ u ... name)) # from (users # as u)
                  TM.notParameterized """SELECT DISTINCT "u"."id" "u.id", "u"."name" "u.name" FROM users AS "u"""" $ Query.query q
                  TM.result q [ { "u.id": 1, "u.name": "josh" }, { "u.id": 2, "u.name": "mary" } ]

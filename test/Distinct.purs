module Test.Distinct where

import Droplet.Language (distinct, from, select)
import Prelude
import Test.Types (id, messages, name, users)

import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests :: TestSuite
tests = do
      TU.suite "distinct" do
            TU.test "field" do
                  let q = select (distinct id) # from messages
                  TM.notParameterized """SELECT DISTINCT id FROM messages""" $ Query.query q
                  TM.result' q []
            TU.test "fields" do
                  let q = select (distinct (id /\ name)) # from users
                  TM.notParameterized """SELECT DISTINCT id, name FROM users""" $ Query.query q
                  TM.result q [{id: 1, name: "josh"}, {id: 2, name: "mary"}]

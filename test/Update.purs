module Test.Update where

import Droplet.Internal.Language.Syntax
import Droplet.Internal.Language.Condition
import Prelude
import Test.Types

import Data.Tuple.Nested ((/\))
import Droplet.Internal.Language.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests :: TestSuite
tests = do
      TU.suite "set" do
            TU.test "single field" do
                  let q = update users # set (surname /\ "Sue")
                  TM.parameterized "UPDATE users SET surname = $1" $ Query.query q
                  TM.result q []
            TU.test "fields" do
                  let q = update users # set ((name /\ "Mary") /\ (surname /\ "Sue"))
                  TM.parameterized "UPDATE users SET name = $1, surname = $2" $ Query.query q
                  TM.result q []
      TU.suite "where" do
            TU.test "single field" do
                  let q = update users # set (surname /\ "Sue") # wher (id .=. 1)
                  TM.parameterized "UPDATE users SET surname = $1 WHERE id = $2" $ Query.query q
                  TM.result q []
            TU.test "fields" do
                  let q = update users # set ((name /\ "Mary") /\ (surname /\ "Sue")) # wher (id .=. 2 .||. id .=. 4)
                  TM.parameterized "UPDATE users SET name = $1, surname = $2 WHERE (id = $3 OR id = $4)" $ Query.query q
                  TM.result q []

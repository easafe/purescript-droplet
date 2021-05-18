module Test.Update where

import Droplet.Internal.Edsl.Language
import Prelude
import Test.Types

import Data.Tuple.Nested ((/\))
import Droplet.Internal.Mapper.Query as Query
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
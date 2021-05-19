module Test.Unsafe where

import Droplet.Internal.Edsl.Language
import Droplet.Internal.Edsl.Condition
import Prelude
import Test.Types

import Data.Tuple.Nested ((/\))
import Droplet.Internal.Mapper.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests :: TestSuite
tests = do
      TU.test "update" do
            let q = update users # set (surname /\ "Sue")
            TM.parameterized "UPDATE users SET surname = $1" $ Query.query q
            TM.result q []

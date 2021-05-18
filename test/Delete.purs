module Test.Delete where

import Droplet.Internal.Edsl.Language
import Prelude
import Test.Types

import Droplet.Internal.Mapper.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests :: TestSuite
tests = do
      TU.suite "delete" do
            TU.test "all" do
                  let q = delete # from users
                  TM.notParameterized "DELETE FROM users" $ Query.query q
                  TM.result' q []

module Test.Delete where

import Droplet.Language
import Prelude
import Test.Types

import Droplet.Language.Internal.Query as DLIQ
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests ∷ TestSuite
tests = do
      TU.suite "delete" do
            TU.test "all" do
                  let q = delete # from users
                  TM.notParameterized """DELETE FROM users""" $ DLIQ.buildQuery q
                  TM.result' q []
            TU.test "where" do
                  let q = delete # from users # wher (id .=. 3)
                  TM.parameterized """DELETE FROM users WHERE "id" = $1""" $ DLIQ.buildQuery q
                  TM.result' q []

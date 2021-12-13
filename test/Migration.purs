module Test.Migration where

import Prelude
import Test.Types

import Droplet.Driver.Migration as DDM
import Test.Model as TM
import Test.Spec (Spec)
import Test.Spec as TS


tests ∷ Spec Unit
tests = TS.describe "migration" do
      TS.describe "runs all migartions" do
            TS.pending "runs all migrations"
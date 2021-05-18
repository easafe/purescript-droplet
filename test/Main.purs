module Test.Main where

import Prelude

import Effect (Effect)
import Test.Insert as TI
import Test.Select as TS
import Test.Unit.Main as TUM
import Test.Update as TU

main :: Effect Unit
main = TUM.runTest do
      TS.tests
      TI.tests
      TU.tests
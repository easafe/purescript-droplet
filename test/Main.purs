module Test.Main where

import Prelude

import Effect (Effect)
import Test.Delete as TD
import Test.Insert as TI
import Test.Returning as TR
import Test.Select as TS
import Test.Transaction as TST
import Test.Unit.Main as TUM
import Test.Unsafe as TUS
import Test.Update as TU

main :: Effect Unit
main = TUM.runTest do
      TUS.tests
      TI.tests
      TU.tests
      TD.tests
      TR.tests
      TS.tests
      TST.tests
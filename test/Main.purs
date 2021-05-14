module Test.Main where

import Prelude

import Test.Select as TS
import Test.Insert as TI
import Effect (Effect)
import Test.Unit.Main as TUM

main :: Effect Unit
main = TUM.runTest do
      TS.tests
      TI.tests
module Test.Main where

import Prelude

import Effect (Effect)
import Test.As as TA
import Test.Delete as TD
import Test.From as TF
import Test.Function as TFC
import Test.Insert as TI
import Test.Limit as TL
import Test.NakedSelect as TNS
import Test.OrderBy as TO
import Test.Returning as TR
import Test.Transaction as TST
import Test.Unit.Main as TUM
import Test.Unsafe as TUS
import Test.Update as TU
import Test.Where as TW

main :: Effect Unit
main = TUM.runTest do
      TUS.tests
      TI.tests
      TU.tests
      TD.tests
      TR.tests
      TF.tests
      TW.tests
      TA.tests
      TO.tests
      TFC.tests
      TL.tests
      TNS.tests
      TST.tests
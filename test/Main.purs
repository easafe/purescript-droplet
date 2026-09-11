module Test.Main where

import Prelude

import Effect (Effect)
import Test.Alter as TAL
import Test.As as TA
import Test.Create as TC
import Test.Delete as TD
import Test.Distinct as TDS
import Test.Drop as TDR
import Test.From as TF
import Test.Function as TFC
import Test.GroupBy as TG
import Test.Insert as TI
import Test.Join as TJ
import Test.Limit as TL
import Test.Migration as TMG
import Test.NakedSelect as TNS
import Test.Offset as TOF
import Test.OrderBy as TO
import Test.Returning as TR
import Test.Spec.Reporter.Tap (tapReporter)
import Test.Spec.Runner.Node as TSR
import Test.SubQuery as TSQ
import Test.Transaction as TST
import Test.Union as TUN
import Test.Unsafe as TUS
import Test.Update as TU
import Test.Where as TW

main ∷ Effect Unit
main = TSR.runSpecAndExitProcess [ tapReporter ] do
      TUS.tests
      TI.tests
      TU.tests
      TD.tests
      TR.tests
      TF.tests
      TDS.tests
      TJ.tests
      TAL.tests
      TW.tests
      TUN.tests
      TG.tests
      TA.tests
      TSQ.tests
      TMG.tests
      TOF.tests
      TDR.tests
      TO.tests
      TFC.tests
      TL.tests
      TNS.tests
      TST.tests
      TC.tests

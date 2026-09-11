module Test.RunTestFailure where

import Prelude

import Data.String (Pattern(..))
import Data.String as DS
import Effect.Aff (Aff)
import Effect.Class as EC
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Test.Spec.Assertions as TSA

type BuildResult = { exitCode ∷ Int, stderr ∷ String }

foreign import build_ ∷ EffectFn1 String BuildResult

build ∷ String → Aff BuildResult
build = EC.liftEffect <<< EU.runEffectFn1 build_

-- | Build a package that is expected to have type errors
testFailure ∷ String → String → String → Aff Unit
testFailure package errorName details = do
      result ← build package
      pure unit
      when (result.exitCode == 0) <<< TSA.fail $ "Expected " <> package <> " to fail to compile, but it succeeded."
      unless (DS.contains (Pattern errorName) result.stderr) <<< TSA.fail $ "Expected type error " <> errorName <> " but got:\n" <> result.stderr
      unless (DS.contains (Pattern details) result.stderr) <<< TSA.fail $ "Expected type error message to mention " <> details <> " but got:\n" <> result.stderr
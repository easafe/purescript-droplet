module Test.Delete where

import Droplet.Language
import Prelude
import Test.Types

import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM

import Test.Spec (Spec)
import Test.Spec as TS


tests ∷ Spec Unit
tests = do
      TS.describe "delete" do
            TS.it "all" do
                  let q = delete # from users
                  TM.notParameterized """DELETE FROM "users"""" $ DLIQ.buildQuery q
                  TM.result' q []
            TS.it "where" do
                  let q = delete # from users # wher (id .=. 3)
                  TM.parameterized """DELETE FROM "users" WHERE "id" = $1""" $ DLIQ.buildQuery q
                  TM.result' q []

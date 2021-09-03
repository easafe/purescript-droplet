module Test.Insert where

import Droplet.Language
import Prelude
import Test.Types

import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests ∷ TestSuite
tests = do
      TU.suite "insert" do
            TU.suite "values" do
                  TU.test "all fields" do
                        let q = insert # into users (name /\ surname /\ birthday /\ joined) # values ("mary" /\ "m." /\ TM.makeDate 2000 9 9 /\ TM.makeDate 2009 9 9)
                        TM.parameterized """INSERT INTO users(name, surname, birthday, joined) VALUES ($1, $2, $3, $4)""" $ Query.query q
                        TM.result' q []
                  TU.test "some fields" do
                        let q = insert # into tags name # values "my tag"
                        TM.parameterized """INSERT INTO tags(name) VALUES ($1)""" $ Query.query q
                        TM.result' q []
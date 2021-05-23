module Test.Returning where

import Droplet.Internal.Language.Syntax
import Prelude
import Test.Types

import Data.Tuple.Nested ((/\))
import Droplet.Internal.Language.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests :: TestSuite
tests = do
      TU.suite "returning" do
            TU.test "insert" do
                  let q = insert # into users (name /\ surname /\ birthday /\ joined) # values ("mary" /\ "m." /\ TM.makeDate 2000 9 9 /\ TM.makeDate 2009 9 9) # returning id
                  TM.parameterized "INSERT INTO users(name, surname, birthday, joined) VALUES ($1, $2, $3, $4) RETURNING id" $ Query.query q
                  TM.result' q [{id: 1}]
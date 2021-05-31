module Test.Function where

import Droplet.Language
import Prelude
import Test.Types

import Data.BigInt as DB
import Droplet.Language.Internal.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests :: TestSuite
tests =
      TU.suite "count" do
            TU.test "star" do
                  let q = select (count star # as u) # from users
                  TM.notParameterized """SELECT COUNT(*) AS "u" FROM users""" $ Query.query q
                  TM.result q [{ u : DB.fromInt 2 }]
            TU.test "field" do
                  let q = select (count id # as u) # from users
                  TM.notParameterized """SELECT COUNT(id) AS "u" FROM users""" $ Query.query q
                  TM.result q [{ u : DB.fromInt 2}]


module Test.Function where

import Droplet.Language
import Prelude
import Test.Types

import Data.BigInt as DB
import Data.Maybe (Maybe(..))
import Droplet.Language.Internal.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests :: TestSuite
tests = do
      TU.suite "count" do
            TU.test "star" do
                  let q = select (count star # as u) # from users
                  TM.notParameterized """SELECT COUNT(*) AS "u" FROM users""" $ Query.query q
                  TM.result q [{ u : DB.fromInt 2 }]
            TU.test "field" do
                  let q = select (count id # as u) # from users
                  TM.notParameterized """SELECT COUNT(id) AS "u" FROM users""" $ Query.query q
                  TM.result q [{ u : DB.fromInt 2}]
            TU.test "path" do
                  let q = select (count (u ... id) # as u) # from (users # as u)
                  TM.notParameterized """SELECT COUNT("u".id) AS "u" FROM users AS "u"""" $ Query.query q
                  TM.result q [{ u : DB.fromInt 2}]
      TU.suite "string_agg" do
            TU.test "field" do
                  let q = select (string_agg name ", " # as u) # from users
                  TM.notParameterized """SELECT string_agg(name, ', ') AS "u" FROM users""" $ Query.query q
                  TM.result q [{ u : Just "josh, mary"}]
            TU.test "path" do
                  let q = select (string_agg (u ... name) ", " # as u) # from (users # as u)
                  TM.notParameterized """SELECT string_agg("u".name, ', ') AS "u" FROM users AS "u"""" $ Query.query q
                  TM.result q [{ u : Just "josh, mary"}]
            TU.suite "order by" do
                  TU.test "field" do
                        let q = select (string_agg name (", " # orderBy id) # as u) # from users
                        TM.notParameterized """SELECT string_agg(name, ', ' ORDER BY id) AS "u" FROM users""" $ Query.query q
                        TM.result q [{ u : Just "josh, mary"}]
                  TU.test "path" do
                        let q = select (string_agg (u ... name) (", " # orderBy (u ... id)) # as u) # from (users # as u)
                        TM.notParameterized """SELECT string_agg("u".name, ', ' ORDER BY "u".id) AS "u" FROM users AS "u"""" $ Query.query q
                        TM.result q [{ u : Just "josh, mary"}]

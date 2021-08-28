module Test.Function where

import Droplet.Language
import Prelude
import Test.Types

import Data.BigInt as DB
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ( (/\))
import Droplet.Language.Internal.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests :: TestSuite
tests = do
      TU.suite "user defined" do
            TU.test "value" do
                  let q = select (date_part_age ("year" /\ (TM.makeDateTime 1900 2 2)) # as u) # from users
                  TM.parameterized """SELECT date_part_age($1, $2) AS "u" FROM users""" $ Query.query q
                  TM.result' q []
            TU.test "field" do
                  let q = select (date_part_age ("month" /\ date) # as u) # from messages
                  TM.parameterized """SELECT date_part_age($1, date) AS "u" FROM messages""" $ Query.query q
                  TM.result' q []
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
                  TM.parameterized """SELECT string_agg(name, $1) AS "u" FROM users""" $ Query.query q
                  TM.result q [{ u : Just "josh, mary"}]
            TU.test "path" do
                  let q = select (string_agg (u ... name) ", " # as u) # from (users # as u)
                  TM.parameterized """SELECT string_agg("u".name, $1) AS "u" FROM users AS "u"""" $ Query.query q
                  TM.result q [{ u : Just "josh, mary"}]
            TU.suite "order by" do
                  TU.test "field" do
                        let q = select (string_agg name (", " # orderBy id) # as u) # from users
                        TM.parameterized """SELECT string_agg(name, $1 ORDER BY id) AS "u" FROM users""" $ Query.query q
                        TM.result q [{ u : Just "josh, mary"}]
                  TU.test "path" do
                        let q = select (string_agg (u ... name) (", " # orderBy (u ... id)) # as u) # from (users # as u)
                        TM.parameterized """SELECT string_agg("u".name, $1 ORDER BY "u".id) AS "u" FROM users AS "u"""" $ Query.query q
                        TM.result q [{ u : Just "josh, mary"}]

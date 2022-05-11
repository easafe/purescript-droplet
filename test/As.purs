module Test.As where

import Droplet.Language
import Prelude
import Test.Types (b, id, messages, n, name, t, u, users)

import Data.Maybe (Maybe(..))
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM

import Test.Spec (Spec)
import Test.Spec as TS
import Type.Proxy (Proxy(..))

tests ∷ Spec Unit
tests =
      TS.describe "as" do
            TS.describe "named field" do
                  TS.it "casing" do
                        let q = select (id # as (Proxy ∷ Proxy "AbCd")) # from users
                        TM.notParameterized """SELECT "id" AS "AbCd" FROM "users"""" $ DLIQ.buildQuery q
                        TM.result q [ { "AbCd": 1 }, { "AbCd": 2 } ]
            TS.describe "named table" do
                  TS.it "field" do
                        let q = select id # from (users # as u)
                        TM.notParameterized """SELECT "id" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 1 }, { id: 2 } ]
                  TS.it "named aliased field" do
                        let q = select (u ... id # as id) # from (users # as u)
                        TM.notParameterized """SELECT "u"."id" AS "id" FROM "users" AS "u"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 1 }, { id: 2 } ]
            TS.describe "named queries" do
                  TS.it "subquery column" do
                        let q = select (select id # from users # wher (name .=. "mary") # orderBy id # limit (Proxy :: _ 1) # as b) # from users # wher (id .=. 1 .||. id .=. 2)
                        TM.parameterized """SELECT (SELECT "id" FROM "users" WHERE "name" = $1 ORDER BY "id" LIMIT 1) AS "b" FROM "users" WHERE ("id" = $2 OR "id" = $3)""" $ DLIQ.buildQuery q
                        TM.result q [ { b: Just 2 }, { b: Just 2 } ]
                  TS.it "scalar" do
                        let q = select (4 # as n) # from (select (4 # as n) # from users # wher (id .=. id) # as u)
                        TM.notParameterized """SELECT 4 AS "n" FROM (SELECT 4 AS "n" FROM "users" WHERE "id" = "id") AS "u"""" $ DLIQ.buildQuery q
                        TM.result q [ { n: 4 }, { n: 4 } ]
                  TS.it "field" do
                        let q = select id # from (select id # from messages # wher (id .=. id) # as t)
                        TM.notParameterized """SELECT "id" FROM (SELECT "id" FROM "messages" WHERE "id" = "id") AS "t"""" $ DLIQ.buildQuery q
                        TM.result q [ { id: 1 }, { id: 2 } ]
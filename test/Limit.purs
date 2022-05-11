module Test.Limit where

import Droplet.Language (as, from, limit, offset, orderBy, select, wher, (.<>.))
import Prelude
import Test.Types (id, n, name, users)

import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM

import Test.Spec (Spec)
import Test.Spec as TS
import Type.Proxy(Proxy(..))

tests ∷ Spec Unit
tests =
      TS.describe "limit" do
            TS.it "from" do
                  let q = select (4 # as n) # from users # orderBy n # limit (Proxy :: _ 4)
                  TM.notParameterized """SELECT 4 AS "n" FROM "users" ORDER BY "n" LIMIT 4""" $ DLIQ.buildQuery q
                  TM.result q [ { n: 4 }, { n: 4 } ]
            TS.it "where" do
                  let q = select id # from users # wher (id .<>. 10) # orderBy (id /\ name) # limit (Proxy :: _ 2)
                  TM.parameterized """SELECT "id" FROM "users" WHERE "id" <> $1 ORDER BY "id", "name" LIMIT 2""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 1 }, { id: 2 } ]
            TS.it "offset" do
                  let q = select (4 # as n) # from users # orderBy n # offset 4 # limit (Proxy :: _ 5)
                  TM.notParameterized """SELECT 4 AS "n" FROM "users" ORDER BY "n" OFFSET 4 LIMIT 5""" $ DLIQ.buildQuery q
                  TM.result q []

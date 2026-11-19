module Test.Columns where

import Droplet.Language
import Prelude hiding (join)
import Test.Types

import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM
import Test.Spec (Spec)
import Test.Spec as TS

tests ∷ Spec Unit
tests =
      TS.describe "column names" do
            TS.it "columns" do
                  let usr = columns users
                  let q = select (usr.id /\ usr.name) # from users # wher (usr.name .=. "mary")
                  TM.parameterized """SELECT "id", "name" FROM "users" WHERE "name" = $1""" $ DLIQ.buildQuery q
                  TM.result q [ { id: 2, name: "mary" } ]
            TS.it "qualified" do
                  let u = qualified @"u" users
                  let m = qualified @"m" messages
                  let q = select (u.name /\ (m.id # as @"message")) # from ((users # as @"u") `join` (messages # as @"m") # on (u.id .=. m.sender)) # orderBy m.id
                  TM.notParameterized """SELECT "u"."name" "u.name", "m"."id" AS "message" FROM "users" AS "u" INNER JOIN "messages" AS "m" ON "u"."id" = "m"."sender" ORDER BY "m"."id"""" $ DLIQ.buildQuery q
                  TM.result q [ { "u.name": "josh", message: 1 }, { "u.name": "mary", message: 2 } ]
            TS.it "col" do
                  let q = select (col @"n") # from (select (id # as @"n") # from users # as @"t") # orderBy (col @"n")
                  TM.notParameterized """SELECT "n" FROM (SELECT "id" AS "n" FROM "users") AS "t" ORDER BY "n"""" $ DLIQ.buildQuery q
                  TM.result q [ { n: 1 }, { n: 2 } ]

module Test.Unsafe where

import Prelude

import Data.Array ((!!))
import Data.Array as DA
import Data.Array.Partial as DAP
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Droplet.Language.Internal.Translate (Query(..))
import Droplet.Language.Internal.Translate as DLIQ
import Foreign as F
import Partial.Unsafe as PU
import Test.Model as TM

import Test.Spec (Spec)
import Test.Spec as TS
import Test.Spec.Assertions  as TSA


tests ∷ Spec Unit
tests = do
      TS.describe "unsafe queries" do
            TS.it "select" do
                  let (plan /\ q /\ pr) = Nothing /\ """SELECT "name", m.id FROM "users" u JOIN "messages" m on "u"."id" = m.sender WHERE "u"."id" = @id OR "u"."id" = @id2 OR "u"."id" = @id3""" /\ { id: 2, id2: 3, id3: 4 }
                  let Query _ dollaredQ parameters = DLIQ.unsafeBuildQuery plan q pr
                  --parameters are replaced by field order
                  TSA.shouldEqual """SELECT "name", m.id FROM "users" u JOIN "messages" m on "u"."id" = m.sender WHERE "u"."id" = $1 OR "u"."id" = $2 OR "u"."id" = $3""" dollaredQ
                  TSA.shouldEqual 3 $ DA.length parameters
                  TSA.shouldEqual pr.id <<< F.unsafeFromForeign $ PU.unsafePartial (DAP.head parameters)
                  TSA.shouldEqual pr.id2 <<< F.unsafeFromForeign $ PU.unsafePartial (DM.fromJust $ parameters !! 1)
                  TSA.shouldEqual pr.id3 <<< F.unsafeFromForeign $ PU.unsafePartial (DM.fromJust $ parameters !! 2)
                  TM.unsafeResult plan q pr [ { name: "mary", id: 2 } ]
            TS.it "insert" do
                  let (plan /\ q) = Nothing /\ "insert into tags(name) values('hey')"
                  let Query _ dollaredQ parameters = DLIQ.unsafeBuildQuery plan q {}
                  TSA.shouldEqual "insert into tags(name) values('hey')" dollaredQ
                  TSA.shouldEqual 0 $ DA.length parameters
                  TM.unsafeResult plan q {} ([] ∷ Array {})
            TS.it "update" do
                  let (plan /\ q /\ pr) = Nothing /\ """UPDATE users SET "name" = @name WHERE "id" = @id""" /\ { name: "Suzy", id: 23 }
                  let Query _ dollaredQ parameters = DLIQ.unsafeBuildQuery plan q pr
                  --parameters are replaced by field order
                  TSA.shouldEqual """UPDATE users SET "name" = $2 WHERE "id" = $1""" dollaredQ
                  TSA.shouldEqual 2 $ DA.length parameters
                  TSA.shouldEqual pr.id <<< F.unsafeFromForeign $ PU.unsafePartial (DAP.head parameters)
                  TSA.shouldEqual pr.name <<< F.unsafeFromForeign $ PU.unsafePartial (DM.fromJust $ parameters !! 1)
                  TM.unsafeResult plan q pr ([] ∷ Array {})
            TS.it "delete" do
                  let (plan /\ q /\ pr) = Nothing /\ """DELETE FROM "users" WHERE joined = @joined""" /\ { joined: TM.makeDate 2000 1 1 }
                  let Query _ dollaredQ parameters = DLIQ.unsafeBuildQuery plan q pr
                  --parameters are replaced by field order
                  TSA.shouldEqual """DELETE FROM "users" WHERE joined = $1""" dollaredQ
                  TSA.shouldEqual 1 $ DA.length parameters
                  TM.unsafeResult plan q pr ([] ∷ Array {})

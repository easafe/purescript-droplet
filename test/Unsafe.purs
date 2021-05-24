module Test.Unsafe where

import Prelude

import Data.Array ((!!))
import Data.Array as DA
import Data.Array.Partial as DAP
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Droplet.Internal.Language.Query (Query(..))
import Droplet.Internal.Language.Query as Query
import Foreign as F
import Partial.Unsafe as PU
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "unsafe queries" do
            TU.test "select" do
                  let (plan /\ q /\ pr) = Nothing /\ "SELECT name, m.id FROM users u JOIN messages m on u.id = m.sender WHERE u.id = @id OR u.id = @id2 OR u.id = @id3" /\ { id: 2, id2: 3, id3: 4 }
                  let Query _ dollaredQ parameters = Query.unsafeQuery plan q pr
                  --parameters are replaced by field order
                  TUA.equal "SELECT name, m.id FROM users u JOIN messages m on u.id = m.sender WHERE u.id = $1 OR u.id = $2 OR u.id = $3" dollaredQ
                  TUA.equal 3 $ DA.length parameters
                  TUA.equal pr.id <<< F.unsafeFromForeign $ PU.unsafePartial (DAP.head parameters)
                  TUA.equal pr.id2 <<< F.unsafeFromForeign $ PU.unsafePartial (DM.fromJust $ parameters !! 1)
                  TUA.equal pr.id3 <<< F.unsafeFromForeign $ PU.unsafePartial (DM.fromJust $ parameters !! 2)
                  TM.unsafeResult plan q pr [ {name : "mary", id: 2} ]
            TU.test "insert" do
                  let (plan /\ q) = Nothing /\ "insert into tags(name) values('hey')"
                  let Query _ dollaredQ parameters = Query.unsafeQuery plan q {}
                  TUA.equal "insert into tags(name) values('hey')" dollaredQ
                  TUA.equal 0 $ DA.length parameters
                  TM.unsafeResult plan q {} ([] :: Array {})
            TU.test "update" do
                  let (plan /\ q /\ pr) = Nothing /\ "UPDATE users SET name = @name WHERE id = @id" /\ { name: "Suzy", id: 23 }
                  let Query _ dollaredQ parameters = Query.unsafeQuery plan q pr
                  --parameters are replaced by field order
                  TUA.equal "UPDATE users SET name = $2 WHERE id = $1" dollaredQ
                  TUA.equal 2 $ DA.length parameters
                  TUA.equal pr.id <<< F.unsafeFromForeign $ PU.unsafePartial (DAP.head parameters)
                  TUA.equal pr.name <<< F.unsafeFromForeign $ PU.unsafePartial (DM.fromJust $ parameters !! 1)
                  TM.unsafeResult plan q pr ([] :: Array {})
            TU.test "delete" do
                  let (plan /\ q /\ pr) = Nothing /\ "DELETE FROM users WHERE joined = @joined" /\ { joined: TM.makeDate 2000 1 1 }
                  let Query _ dollaredQ parameters = Query.unsafeQuery plan q pr
                  --parameters are replaced by field order
                  TUA.equal "DELETE FROM users WHERE joined = $1" dollaredQ
                  TUA.equal 1 $ DA.length parameters
                  TM.unsafeResult plan q pr ([] :: Array {})


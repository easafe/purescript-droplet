module Test.Unsafe where

import Droplet.Internal.Edsl.Language
import Prelude
import Test.Types

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Internal.Mapper.Query (Query(..))
import Droplet.Internal.Mapper.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "unsafe queries" do
            TU.test "select" do
                  let (plan /\ q /\ parameters) = Nothing /\ "UPDATE users SET name = @name WHERE id = @id" /\ { name: "Suzy", id: 23 }
                  let Query _ dollaredQ _ = Query.unsafeQuery plan q parameters
                  --parameters are replaced by field order
                  TUA.equal "UPDATE users SET name = $2 WHERE id = $1" dollaredQ
                 -- TM.unsafeResult plan q parameters

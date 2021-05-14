module Test.Insert where

import Droplet.Internal.Edsl.Condition
import Droplet.Internal.Edsl.Definition
import Droplet.Internal.Edsl.Language
import Prelude
import Test.Types

import Data.Date (Date)
import Data.Date as DD
import Data.DateTime (DateTime(..), Time(..))
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum)
import Data.Enum as DE
import Data.Eq (class EqRecord)
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Show (class ShowRecordFields)
import Data.Tuple.Nested ((/\))
import Droplet.Internal.Mapper.Driver (class FromResult)
import Droplet.Internal.Mapper.Driver as Driver
import Droplet.Internal.Mapper.Pool as DIMP
import Droplet.Internal.Mapper.Query (class ToQuery, Query(..))
import Droplet.Internal.Mapper.Query as Query
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Partial.Unsafe as PU
import Prim.RowList (class RowToList)
import Test.Main as TM
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "values" do
            TU.test "all fields" do
                  let q = insertInto users (id /\ name /\ surname /\ birthday /\ joined) # values (3 /\ "mary" /\ "m." /\ TM.makeDate 2000 9 9 /\ TM.makeDate 2009 9 9)
                  TM.parameterized "INSERT INTO users VALUES ($1, $2, $3, $4, $5)" $ Query.query q
                  TM.result q []
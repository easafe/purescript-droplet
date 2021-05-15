module Test.Model where

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
import Droplet (class ToQuery, Query(..))
import Droplet.Internal.Mapper.Driver (class FromResult, Connection, PgError)
import Droplet.Internal.Mapper.Driver as Driver
import Droplet.Internal.Mapper.Pool (Configuration)
import Droplet.Internal.Mapper.Pool as DIMP
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Partial.Unsafe as PU
import Prim.RowList (class RowToList)
import Test.Unit as TU
import Test.Unit.Assert as TUA

makeDate :: Int -> Int -> Int -> Date
makeDate y m d = DD.canonicalDate (unsafeToEnum y) (unsafeToEnum m) (unsafeToEnum d)

makeDateTime :: Int -> Int -> Int -> DateTime
makeDateTime y m d = DateTime (makeDate y m d) $ Time (unsafeToEnum 0) (unsafeToEnum 0) (unsafeToEnum 0) (unsafeToEnum 0)

unsafeToEnum :: forall a. BoundedEnum a => Int -> a
unsafeToEnum v = PU.unsafePartial (DM.fromJust $ DE.toEnum v)

notParameterized :: forall projection. String -> Query projection -> _
notParameterized s (Query _ q parameters) = case parameters of
      [] -> TUA.equal s q
      _ -> TU.failure $ "Expected no parameters for " <> s

parameterized :: forall projection. String -> Query projection -> _
parameterized s (Query _ q parameters) = case parameters of
      [] -> TU.failure $ "Expected parameters for " <> s
      _ -> TUA.equal s q

-- | Compares query return to expected result
-- |
-- | Default records are inserted before running the query. Use result' if that's not the intended behavior
result :: forall t51 t52 t53. ToQuery t51 t52 => RowToList t52 t53 => FromResult t53 (Record t52) => EqRecord t53 t52 => ShowRecordFields t53 t52 => t51 -> Array (Record t52) -> Aff Unit
result q o = do
      pool <- liftEffect $ DIMP.new connectionInfo
      Driver.withConnection pool case _ of
            Left error -> TU.failure $ "Error connecting" <> show error
            Right connection -> do
                  insertDefaultRecords
                  r <- Driver.query connection q
                  TUA.equal (Right o) r
                  truncateTables connection

result' :: forall t51 t52 t53. ToQuery t51 t52 => RowToList t52 t53 => FromResult t53 (Record t52) => EqRecord t53 t52 => ShowRecordFields t53 t52 => t51 -> Array (Record t52) -> Aff Unit
result' q o = do
      pool <- liftEffect $ DIMP.new connectionInfo
      Driver.withConnection pool case _ of
            Left error -> TU.failure $ "Error connecting" <> show error
            Right connection -> do
                  r <- Driver.query connection q
                  TUA.equal (Right o) r
                  truncateTables connection

truncateTables :: Connection -> Aff Unit
truncateTables connection = void (Driver.unsafeQuery connection "select truncate_tables()" [] :: Aff (Either PgError (Array {})))

insertDefaultRecords :: Aff Unit
insertDefaultRecords = do
      pool <- liftEffect $ DIMP.new connectionInfo
      Driver.withConnection pool case _ of
            Left error -> TU.failure $ "Error connecting" <> show error
            Right connection -> do
                  pure unit
                  -- void <<< Driver.query connection $ insertInto users (name /\ surname /\ birthday) # values ("josh" /\ "j." /\ makeDate 1990 1 1)
                  -- void <<< Driver.query connection $ insertInto users (name /\  surname /\ birthday) # values ("mary" /\ "sue" /\ makeDate 1900 11 11)
                  -- void <<< Driver.query connection $ insertInto messages (sender /\  recipient /\ sent /\ date /\ secondDate) # values (1 /\ 2 /\ true /\ makeDateTime 2000 3 4 /\ makeDateTime 2000 3 4)
                  -- void <<< Driver.query connection $ insertInto messages (sender /\ recipient /\ sent /\ date /\ secondDate) # values (2 /\ 1 /\ true /\ makeDateTime 2000 3 4 /\ makeDateTime 2000 3 4)

connectionInfo :: Configuration
connectionInfo = (DIMP.defaultConfiguration "droplet") {
            user = Just "droplet",
            host = Nothing,
            idleTimeoutMillis = Just 1000
      }
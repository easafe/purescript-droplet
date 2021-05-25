module Test.Model where

import Droplet.Language
import Prelude
import Test.Types

import Data.Date (Date)
import Data.Date (canonicalDate) as DD
import Data.DateTime (DateTime(..), Time(..))
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum)
import Data.Enum as DE
import Data.Eq (class EqRecord)
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Show (class ShowRecordFields)
import Data.Tuple.Nested ((/\))
import Droplet.Driver (class FromResult,  Configuration, Connection, PgError)
import Droplet.Language.Internal.Query(class ToQuery, Query(..))
import Droplet.Driver (defaultConfiguration, newPool, query, withConnection) as DD
import Droplet.Driver.Unsafe as DDU
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
      pool <- liftEffect $ DD.newPool connectionInfo
      DD.withConnection pool case _ of
            Left error -> TU.failure $ "Error connecting" <> show error
            Right connection -> do
                  insertDefaultRecords
                  r <- DD.query connection q
                  TUA.equal (Right o) r
                  truncateTables connection

result' :: forall t51 t52 t53. ToQuery t51 t52 => RowToList t52 t53 => FromResult t53 (Record t52) => EqRecord t53 t52 => ShowRecordFields t53 t52 => t51 -> Array (Record t52) -> Aff Unit
result' q o = do
      pool <- liftEffect $ DD.newPool connectionInfo
      DD.withConnection pool case _ of
            Left error -> TU.failure $ "Error connecting" <> show error
            Right connection -> do
                  r <- DD.query connection q
                  TUA.equal (Right o) r
                  truncateTables connection

unsafeResult :: forall re parameters par result. RowToList result re  => RowToList parameters par => ToParameters parameters par => FromResult re (Record result) => EqRecord re result => ShowRecordFields re result => Maybe Plan -> String -> Record parameters -> Array (Record result) -> Aff Unit
unsafeResult plan q parameters o = do
      pool <- liftEffect $ DD.newPool connectionInfo
      DD.withConnection pool case _ of
            Left error -> TU.failure $ "Error connecting" <> show error
            Right connection -> do
                  insertDefaultRecords
                  r <- DDU.unsafeQuery connection plan q parameters
                  TUA.equal (Right o) r
                  truncateTables connection

truncateTables :: Connection -> Aff Unit
truncateTables connection = void (DDU.unsafeQuery connection Nothing "select truncate_tables()" {} :: Aff (Either PgError (Array {})))

insertDefaultRecords :: Aff Unit
insertDefaultRecords = do
      pool <- liftEffect $ DD.newPool connectionInfo
      DD.withConnection pool case _ of
            Left error -> TU.failure $ "Error connecting" <> show error
            Right connection -> do
                  pure unit
                  void <<< DD.query connection $ insert # into users (name /\ surname /\ birthday) # values ("josh" /\ "j." /\ makeDate 1990 1 1)
                  void <<< DD.query connection $ insert # into users (name /\ surname /\ birthday) # values ("mary" /\ "sue" /\ makeDate 1900 11 11)
                  void <<< DD.query connection $ insert # into messages (sender /\ recipient /\ sent /\ date /\ secondDate) # values (1 /\ 2 /\ true /\ makeDateTime 2000 3 4 /\ makeDateTime 2000 3 4)
                  void <<< DD.query connection $ insert # into messages (sender /\ recipient /\ sent /\ date /\ secondDate) # values (2 /\ 1 /\ true /\ makeDateTime 2000 3 4 /\ makeDateTime 2000 3 4)
                  void <<< DD.query connection $ insert # into tags (name /\ _by) # values ("tagged" /\ Just 1)

connectionInfo :: Configuration
connectionInfo = (DD.defaultConfiguration "droplet") {
            user = Just "droplet",
            host = Nothing,
            idleTimeoutMillis = Just 1000
      }
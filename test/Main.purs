module Test.Main where

import Droplet.Internal.Edsl.Definition
import Droplet.Internal.Edsl.Condition
import Droplet.Internal.Edsl.Language
import Prelude

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
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Partial.Unsafe as PU
import Prim.RowList (class RowToList)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM

type Users = (id :: Int, name :: String, surname :: String, birthday :: Date, joined :: Date)
type Messages = (id :: Int, sender :: Int, recipient :: Int, date :: DateTime, sent :: Boolean)

users :: Table "users" Users
users = Table

messages :: Table "messages" Messages
messages = Table

id :: Field "id"
id = Field

name :: Field "name"
name = Field

surname :: Field "surname"
surname = Field

sent :: Field "sent"
sent = Field

date :: Field "date"
date = Field

joined :: Field "joined"
joined = Field

birthday :: Field "birthday"
birthday = Field

b :: Alias "b"
b = Alias

n :: Alias "n"
n = Alias

t :: Alias "t"
t = Alias

u :: Alias "u"
u = Alias

--lets clean these test out of these meaningless suie categories
main :: Effect Unit
main = TUM.runTest do
      TU.suite "from" do
            TU.test "table" do
                  let q = (select (34 # as b) # from messages)
                  notParameterized "SELECT 34 AS b FROM messages" $ Query.query q
                  result q [{b : 34}, {b : 34}]

      TU.suite "parameters" do
            TU.test "equals" do
                  let q = select id # from users # wher (name .=. "josh")
                  parameterized "SELECT id FROM users WHERE name = $1" $ Query.query q
                  result q [{id : 1}]
            TU.test "not equals" do
                  let q = select id # from users # wher (id .<>. 3)
                  parameterized "SELECT id FROM users WHERE id <> $1" $ Query.query q
                  result q [{id : 1}, {id: 2}]

            TU.suite "field" do
                  TU.test "equals" do
                        let q = select (34 # as n) # from users # wher (name .=. surname)
                        notParameterized "SELECT 34 AS n FROM users WHERE name = surname" $ Query.query q
                        result q []
                  TU.test "not equals" do
                        let q = select (34 # as n) # from users # wher (name .<>. surname)
                        notParameterized "SELECT 34 AS n FROM users WHERE name <> surname" $ Query.query q
                        result q [{n : 34}, {n: 34}]

            TU.suite "logical operands" do
                  TU.suite "and" do
                        TU.test "single" do
                              let q = select id # from users # wher (name .=. "josh" .&&. name .<>. surname)
                              parameterized "SELECT id FROM users WHERE (name = $1 AND name <> surname)" $ Query.query q
                              result q [{id : 1}]
                        TU.test "many" do
                              let q = select id # from users # wher (name .=. "josh" .&&. "josh" .=. name .&&. surname .=. "j.")
                              parameterized "SELECT id FROM users WHERE ((name = $1 AND $2 = name) AND surname = $3)" $ Query.query q
                              result q [{id : 1}]

                  TU.suite "or" do
                        TU.test "single" do
                              let q = select id # from users # wher (name .=. "mary" .||. name .=. surname)
                              parameterized "SELECT id FROM users WHERE (name = $1 OR name = surname)" $ Query.query q
                              result q [{id : 2}]
                        TU.test "many" do
                              let q = select id # from users # wher (name .=. "josh" .||. name .=. "j." .||. surname .<>. "josh")
                              parameterized "SELECT id FROM users WHERE ((name = $1 OR name = $2) OR surname <> $3)" $ Query.query q
                              result q [{id : 1}, {id : 2}]

                  TU.suite "tuple" do
                        TU.test "not bracketed" do
                              let q = select id # from users # wher (id .=. 333 .||. id .=. 33 .&&. id .=. 3)
                              parameterized "SELECT id FROM users WHERE (id = $1 OR (id = $2 AND id = $3))" $ Query.query q
                              result q []
                        TU.test "bracketed" do
                              let q = select id # from users # wher ((id .=. 2 .||. id .=. 22) .&&. id .=. 2)
                              parameterized "SELECT id FROM users WHERE ((id = $1 OR id = $2) AND id = $3)" $ Query.query q
                              result q [{id: 2 }]

            TU.suite "subqueries" do
                  TU.test "scalar" do
                        let namep = "mary"
                        let q = select (select (4 # as n) # from users # wher (name .=. namep) # as b)
                        parameterized "SELECT (SELECT 4 AS n FROM users WHERE name = $1) AS b" $ Query.query q
                        result q [{b: 4 }]
                  TU.test "field" do
                        let namep = "josh"
                        let q = select (select id # from users # wher (name .=. namep) # as b)
                        parameterized "SELECT (SELECT id FROM users WHERE name = $1) AS b" $ Query.query q
                        result q [{b: 1 }]
                  TU.test "tuple" do
                        let parameters = { d : "mary", e : 2 }
                        let q = select ((3 # as (Alias :: Alias "e")) /\ (select id # from users # wher (name .=. parameters.d) # as b) /\ (select id # from messages # wher (id .=. parameters.e) # as n))
                        parameterized "SELECT 3 AS e, (SELECT id FROM users WHERE name = $1) AS b, (SELECT id FROM messages WHERE id = $2) AS n" $ Query.query q
                        result q [{e: 3, b: 2, n: 2 }]
                  TU.test "where" do
                        let parameters = { d : "mary", e : 2 }
                        let q = select ((3 # as (Alias :: Alias "e")) /\ (select id # from users # wher (name .=. parameters.d) # as b) /\ (select id # from messages # wher (id .=. parameters.e) # as n)) # from users # wher (id .=. 1 .||. id .=. 2)
                        parameterized "SELECT 3 AS e, (SELECT id FROM users WHERE name = $1) AS b, (SELECT id FROM messages WHERE id = $2) AS n FROM users WHERE (id = $3 OR id = $4)" $ Query.query q
                        result q [{e: 3, b: 2, n: 2 }, {e: 3, b: 2, n: 2 }]

      TU.suite "as" do
            TU.suite "from" do
                  TU.test "scalar" do
                        let q = select (4 # as n) # from (select (4 # as n) # from messages # as (Alias :: Alias "u"))
                        notParameterized "SELECT 4 AS n FROM (SELECT 4 AS n FROM messages) AS u" $ Query.query q
                        result q [{n: 4}, {n: 4}]
                  TU.test "field" do
                        let q = select birthday # from (select birthday # from users # as t)
                        notParameterized "SELECT birthday FROM (SELECT birthday FROM users) AS t" $ Query.query q
                        result q [{birthday: makeDate 1990 1 1}, {birthday: makeDate 1900 11 11}]
                  TU.test "renamed field" do
                        let q = select (Field :: Field "t") # from (select (birthday # as t) # from users # as t)
                        notParameterized "SELECT t FROM (SELECT birthday AS t FROM users) AS t" $ Query.query q
                        result q [{t: makeDate 1990 1 1}, {t: makeDate 1900 11 11}]
                  TU.testSkip "sub query" do
                        let q = select date # from (select (select date # from messages) # from users # as t)
                        --needs limit
                        notParameterized "SELECT date FROM (SELECT (SELECT date FROM messages) FROM users) AS t" $ Query.query q
                        result q [{date: makeDateTime 2000 3 4}, {date: makeDateTime 2000 3 4}]
                  TU.testSkip "tuple" do
                        let q = select (id /\ date /\ (4 # as n) /\ sent) # from (select (id /\ date /\ (4 # as n) /\ (select sent # from messages)) # from messages # as t)
                        notParameterized "SELECT id, date, 4 AS n, sent FROM (SELECT id, date, 4 AS n, (SELECT sent FROM messages) AS sent FROM messages) AS t" $ Query.query q
                        --needs limit
                        result q [{id: 1, date: makeDateTime 2000 3 4, n: 4, sent: true}, {id: 1, date: makeDateTime 2000 3 4, n: 4, sent: true}]

            TU.suite "where" do
                  TU.test "scalar" do
                        let q = select (4 # as n) # from (select (4 # as n) # from users # wher (id .=. id) # as (Alias :: Alias "u"))
                        notParameterized "SELECT 4 AS n FROM (SELECT 4 AS n FROM users WHERE id = id) AS u" $ Query.query q
                        result q [{n: 4}, {n: 4}]
                  TU.test "field" do
                        let q = select id # from (select id # from messages # wher (id .=. id) # as t)
                        notParameterized "SELECT id FROM (SELECT id FROM messages WHERE id = id) AS t" $ Query.query q
                        result q [{id: 1}, {id: 2}]
                  TU.testSkip "sub query" do
                        let q = select (Field :: Field "id") # from (select (select id # from messages # wher (id .=. id)) # from users # wher (id .=. id) # as t)
                        notParameterized "SELECT id FROM (SELECT (SELECT id FROM messages WHERE id = id) FROM users WHERE id = id) AS t" $ Query.query q
                        --needs limit
                        result q [{id: 1}, {id: 2}]
                  TU.testSkip "tuple" do
                        let q = select (id /\ date /\ (4 # as n) /\ sent) # from (select (id /\ date /\ (4 # as n) /\ (select sent # from messages # wher (id .=. id))) # from messages # wher (id .=. id) # as t)
                        notParameterized "SELECT id, date, 4 AS n, sent FROM (SELECT id, date, 4 AS n, (SELECT sent FROM messages WHERE id = id) FROM messages WHERE id = id) AS t" $ Query.query q
                        --needs limit
                        --result q [{id: 1}, {id: 2}]

            TU.suite "parameters" do
                  TU.testSkip "scalar" do
                        let parameters = {date: makeDate 2000 3 4}
                        let q = select (4 # as n) # from (select (4 # as n) # from users # wher (joined .=. parameters.date) # as (Alias :: Alias "u"))
                        parameterized "SELECT 4 AS n FROM (SELECT 4 AS n FROM users WHERE joined = $1) AS u" $ Query.query q
                        result q [{n: 4}, {n: 4}]
                  TU.test "field" do
                        let parameters = {date : makeDateTime 2000 3 4 }
                        let q = select id # from (select id # from messages # wher (date .=. parameters.date) # as t)
                        parameterized "SELECT id FROM (SELECT id FROM messages WHERE date = $1) AS t" $ Query.query q
                        result q [{id: 1}, {id: 2}]
                  TU.testSkip "sub query" do
                        let parameters = {id :1 }
                        let q = select id # from (select (select id # from messages # wher (parameters.id .=. id)) # from users # wher (id .=. parameters.id) # as t)
                        parameterized "SELECT id FROM (SELECT (SELECT id FROM messages WHERE $1 = id) FROM users WHERE id = $2) AS t" $ Query.query q
                        result q [{id: 1}, {id: 2}]
                  TU.testSkip "tuple" do
                        let parameters = {id :3 }
                        --needs limit
                        let q = select (id /\ date /\ (4 # as n) /\ sent) # from (select (id /\ date /\ (4 # as n) /\ (select sent # from messages # wher (parameters.id .=. parameters.id))) # from messages # wher (parameters.id .=. id) # as t)
                        parameterized "SELECT id, date, 4 AS n, sent FROM (SELECT id, date, 4 AS n, (SELECT sent FROM messages WHERE $1 = $1) FROM messages WHERE $1 = id) AS t" $ Query.query q
                        --result q [{id: 1}, {id: 2}]

      TU.suite "naked select" do
            TU.test "scalar" do
                  let q = select (3 # as n)
                  notParameterized "SELECT 3 AS n" $ Query.query q
                  result q [{n : 3}]
            TU.testSkip "sub query" do
                  let q = select (select (34 # as n) # from users # wher (name .=. name) # as t)
                  notParameterized "SELECT (SELECT 34 AS n FROM users WHERE name = name) AS t" $ Query.query q
                  result q [{t : 34}, {t: 34}]
            TU.test "tuple" do
                  --subqueries need to have type Maybe
                  let q = select ((3 # as b) /\ (select (34 # as n) # from users # wher (name .=. surname) # as t) /\ (4 # as (Alias :: Alias "a")) /\ (select name # from users # as (Alias :: Alias "u")))
                  notParameterized "SELECT 3 AS b, (SELECT 34 AS n FROM users WHERE name = surname) AS t, 4 AS a, (SELECT name FROM users) AS u" $ Query.query q
                 -- result q [{b: 3, n: 34, a : 4, name: "mary"}, {b: 3, n: 34, a : 4, name: "mary"}]

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

result :: forall t51 t52 t53. ToQuery t51 t52 => RowToList t52 t53 => FromResult t53 (Record t52) => EqRecord t53 t52 => ShowRecordFields t53 t52 => t51 -> Array (Record t52) -> Aff Unit
result q o = do
      pool <- liftEffect $ DIMP.new (DIMP.defaultConfiguration "droplet") {
            user = Just "droplet",
            host = Nothing,
            idleTimeoutMillis = Just 1000
      }
      Driver.withConnection pool case _ of
            Left error -> TU.failure $ "Error connecting" <> show error
            Right connection -> do
                --  void <<< Driver.query connection $ (NotParameterized "select truncate_tables()" :: Query () ())
                  r <- Driver.query connection q
                  TUA.equal (Right o) r
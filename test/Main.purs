module Test.Main where

import Droplet.Internal.Edsl.Definition
import Droplet.Internal.Edsl.Filter
import Droplet.Internal.Edsl.Language
import Prelude

import Data.Date (Date)
import Data.Date as DD
import Data.DateTime (DateTime(..), Time(..))
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum)
import Data.Enum as DE
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Droplet.Internal.Mapper.Driver as Driver
import Droplet.Internal.Mapper.Pool as DIMP
import Droplet.Internal.Mapper.Query (Query(..))
import Droplet.Internal.Mapper.Query as Query
import Effect (Effect)
import Effect.Class (liftEffect)
import Partial.Unsafe as PU
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

main :: Effect Unit
main = pure unit
-- main = TUM.runTest do
--       TU.suite "from" do
--             TU.test "table" do
--                   let q = (select (34 # as b) # from messages)
--                   notParameterized "SELECT 34 AS b FROM messages" $ Query.query q
--                  result q [{b : 34}, {b : 34}]

--       TU.suite "where" do
--             TU.test "equals" do
--                   let q = select id # from users # wher (name .=. "josh")
--                   parameterized "SELECT id FROM users WHERE name = $1" $ Query.query q
                  --result q [{id : 1}]
      --       TU.test "not equals" do
      --             let parameters = {id: 3}
      --             let q = prepare (Named "test-plan") parameters $ select id # from users # wher (id .<>. idp)
      --             case Query.query q of
      --                   NotParameterized s -> TU.failure $ "Expected parameters for " <> s
      --                   Parameterized _ names s p -> do
      --                         TUA.equal "@id" names
      --                         TUA.equal "SELECT id FROM users WHERE id <> $1" s
      --                         TUA.equal parameters p
      --                         result q [{id : 1}, {id: 2}]

      --       TU.suite "field" do
      --             TU.test "equals" do
      --                   let q = select (34 # as n) # from users # wher (name .=. name)
      --                   notParameterized "SELECT 34 AS n FROM users WHERE name = name" $ Query.query q
      --                   TU.failure "investigate why this needs a type annotation"
      --                   --result q []
      --             TU.test "not equals" do
      --                   let q = select (34 # as n) # from users # wher (name .<>. surname)
      --                   notParameterized "SELECT 34 AS n FROM users WHERE name <> surname" $ Query.query q
      --                   result q [{n : 34}, {n: 34}]

      --       TU.suite "logical operands" do
      --             TU.suite "and" do
      --                   TU.test "single" do
      --                         let parameters = {name : "josh"}
      --                         let q = prepare NotNamed parameters $ select id # from users # wher (name .=. namep .&&. name .<>. surname)
      --                         case Query.query q of
      --                               NotParameterized s -> TU.failure $ "Expected parameters for " <> s
      --                               Parameterized _ names s p -> do
      --                                     TUA.equal "@name" names
      --                                     TUA.equal "SELECT id FROM users WHERE (name = $1 AND name <> surname)" s
      --                                     TUA.equal parameters p
      --                                     result q [{id : 1}]
      --                   TU.test "many" do
      --                         let parameters = {name : "josh", surname: "j."}
      --                         let q = prepare NotNamed parameters $ select id # from users # wher (name .=. namep .&&. namep .=. name .&&. surname .=. (Parameter :: Parameter "surname"))
      --                         case Query.query q of
      --                               NotParameterized s -> TU.failure $ "Expected parameters for " <> s
      --                               Parameterized _ names s p -> do
      --                                     TUA.equal "@name, @surname" names
      --                                     TUA.equal "SELECT id FROM users WHERE ((name = $1 AND $1 = name) AND surname = $2)" s
      --                                     TUA.equal parameters p
      --                                     result q [{id : 1}]

      --             TU.suite "or" do
      --                   TU.test "single" do
      --                         let parameters = {name : "mary"}
      --                         let q = prepare NotNamed parameters $ select id # from users # wher (name .=. namep .||. name .=. surname)
      --                         case Query.query q of
      --                               NotParameterized s -> TU.failure $ "Expected parameters for " <> s
      --                               Parameterized _ names s p -> do
      --                                     TUA.equal "@name" names
      --                                     TUA.equal "SELECT id FROM users WHERE (name = $1 OR name = surname)" s
      --                                     TUA.equal parameters p
      --                                     result q [{id : 2}]
      --                   TU.test "many" do
      --                         let parameters = {name : "josh", surname: "j."}
      --                         let q = prepare NotNamed parameters $ select id # from users # wher (name .=. namep .||. name .=. (Parameter :: Parameter "surname") .||. surname .<>. namep)
      --                         case Query.query q of
      --                               NotParameterized s -> TU.failure $ "Expected parameters for " <> s
      --                               Parameterized _ names s p -> do
      --                                     TUA.equal "@name, @surname" names
      --                                     TUA.equal "SELECT id FROM users WHERE ((name = $1 OR name = $2) OR surname <> $1)" s
      --                                     TUA.equal parameters p
      --                                     result q [{id : 1}, {id : 2}]

      --             TU.suite "tuple" do
      --                   TU.test "not bracketed" do
      --                         let parameters = {id3 : 3, id33: 33, id333 : 333}
      --                         let q = prepare NotNamed parameters $ select id # from users # wher (id .=. (Parameter :: Parameter "id333") .||. id .=. (Parameter :: Parameter "id33") .&&. id .=. (Parameter :: Parameter "id3"))
      --                         case Query.query q of
      --                               NotParameterized s -> TU.failure $ "Expected parameters for " <> s
      --                               Parameterized _ names s p -> do
      --                                     TUA.equal "@id3, @id33, @id333" names
      --                                     TUA.equal "SELECT id FROM users WHERE (id = $3 OR (id = $2 AND id = $1))" s
      --                                     TUA.equal parameters p
      --                                     result q []
      --                   TU.test "bracketed" do
      --                         let parameters = {id3: 2 , id33: 22, id333 : 2}
      --                         let q = prepare NotNamed parameters $ select id # from users # wher ((id .=. (Parameter :: Parameter "id3") .||. id .=. (Parameter :: Parameter "id33")) .&&. id .=. (Parameter :: Parameter "id333"))
      --                         case Query.query q of
      --                               NotParameterized s -> TU.failure $ "Expected parameters for " <> s
      --                               Parameterized _ names s p -> do
      --                                     TUA.equal "@id3, @id33, @id333" names
      --                                     TUA.equal "SELECT id FROM users WHERE ((id = $1 OR id = $2) AND id = $3)" s
      --                                     TUA.equal parameters p
      --                                     result q [{id: 2 }]

      --       TU.suite "prepared subqueries" do
      --             TU.test "scalar" do
      --                   let parameters = { d : "mary" }
      --                   let q = prepare NotNamed parameters $ select (select (4 # as n) # from users # wher (name .=. (Parameter :: Parameter "d")) # as b)
      --                   case Query.query q of
      --                         NotParameterized s -> TU.failure $ "Expected parameters for " <> s
      --                         Parameterized _ names s p -> do
      --                               TUA.equal "@d" names
      --                               TUA.equal "SELECT (SELECT 4 AS n FROM users WHERE name = $1) AS b" s
      --                               TUA.equal parameters p
      --                               result q [{b: 4 }]
      --             TU.test "field" do
      --                   let parameters = { d : "josh" }
      --                   let q = prepare NotNamed parameters (select (select id # from users # wher (name .=. (Parameter :: Parameter "d")) # as b))
      --                   case Query.query q of
      --                         NotParameterized s -> TU.failure $ "Expected parameters for " <> s
      --                         Parameterized _ names s p -> do
      --                               TUA.equal "@d" names
      --                               TUA.equal "SELECT (SELECT id FROM users WHERE name = $1) AS b" s
      --                               TUA.equal parameters p
      --                               result q [{b: 1 }]
      --             TU.test "tuple" do
      --                   let parameters = { d : "mary", e : 2 }
      --                   let q = prepare NotNamed parameters (select ((3 # as (Alias :: Alias "e")) /\ (select id # from users # wher (name .=. (Parameter :: Parameter "d")) # as b) /\ (select id # from messages # wher (id .=. (Parameter :: Parameter "e")) # as n)))
      --                   case Query.query q of
      --                         NotParameterized s -> TU.failure $ "Expected parameters for " <> s
      --                         Parameterized _ names s p -> do
      --                               TUA.equal "@d, @e" names
      --                               TUA.equal "SELECT 3 AS e, (SELECT id FROM users WHERE name = $1) AS b, (SELECT id FROM messages WHERE id = $2) AS n" s
      --                               TUA.equal parameters p
      --                               result q [{e: 3, b: 2, n: 2 }]

      -- TU.suite "as" do
      --       TU.suite "from" do
      --             TU.test "scalar" do
      --                   let q = select (4 # as n) # from (select (4 # as n) # from messages # as (Alias :: Alias "u"))
      --                   notParameterized "SELECT 4 AS n FROM (SELECT 4 AS n FROM messages) AS u" $ Query.query q
      --                   result q [{n: 4}, {n: 4}]
      --             TU.test "field" do
      --                   let q = select birthday # from (select birthday # from users # as t)
      --                   notParameterized "SELECT birthday FROM (SELECT birthday FROM users) AS t" $ Query.query q
      --                   result q [{birthday: makeDate 1990 1 1}, {birthday: makeDate 1900 11 11}]
      --             TU.test "renamed field" do
      --                   let q = select (Field :: Field "t") # from (select (birthday # as t) # from users # as t)
      --                   notParameterized "SELECT t FROM (SELECT birthday AS t FROM users) AS t" $ Query.query q
      --                   result q [{t: makeDate 1990 1 1}, {t: makeDate 1900 11 11}]
      --             TU.testSkip "sub query" do
      --                   let q = select date # from (select (select date # from messages) # from users # as t)
      --                   --needs limit
      --                   notParameterized "SELECT date FROM (SELECT (SELECT date FROM messages) FROM users) AS t" $ Query.query q
      --                   result q [{date: makeDateTime 2000 3 4}, {date: makeDateTime 2000 3 4}]
      --             TU.testSkip "tuple" do
      --                   let q = select (id /\ date /\ (4 # as n) /\ sent) # from (select (id /\ date /\ (4 # as n) /\ (select sent # from messages)) # from messages # as t)
      --                   notParameterized "SELECT id, date, 4 AS n, sent FROM (SELECT id, date, 4 AS n, (SELECT sent FROM messages) AS sent FROM messages) AS t" $ Query.query q
      --                   --needs limit
      --                   result q [{id: 1, date: makeDateTime 2000 3 4, n: 4, sent: true}, {id: 1, date: makeDateTime 2000 3 4, n: 4, sent: true}]

      --       TU.suite "where" do
      --             TU.test "scalar" do
      --                   let q = select (4 # as n) # from (select (4 # as n) # from users # wher (id .=. id) # as (Alias :: Alias "u"))
      --                   notParameterized "SELECT 4 AS n FROM (SELECT 4 AS n FROM users WHERE id = id) AS u" $ Query.query q
      --                   result q [{n: 4}, {n: 4}]
      --             TU.test "field" do
      --                   let q = select id # from (select id # from messages # wher (id .=. id) # as t)
      --                   notParameterized "SELECT id FROM (SELECT id FROM messages WHERE id = id) AS t" $ Query.query q
      --                   result q [{id: 1}, {id: 2}]
      --             TU.testSkip "sub query" do
      --                   let q = select (Field :: Field "id") # from (select (select id # from messages # wher (id .=. id)) # from users # wher (id .=. id) # as t)
      --                   notParameterized "SELECT id FROM (SELECT (SELECT id FROM messages WHERE id = id) FROM users WHERE id = id) AS t" $ Query.query q
      --                   --needs limit
      --                   result q [{id: 1}, {id: 2}]
      --             TU.testSkip "tuple" do
      --                   let q = select (id /\ date /\ (4 # as n) /\ sent) # from (select (id /\ date /\ (4 # as n) /\ (select sent # from messages # wher (id .=. id))) # from messages # wher (id .=. id) # as t)
      --                   notParameterized "SELECT id, date, 4 AS n, sent FROM (SELECT id, date, 4 AS n, (SELECT sent FROM messages WHERE id = id) FROM messages WHERE id = id) AS t" $ Query.query q
      --                   --needs limit
      --                   --result q [{id: 1}, {id: 2}]

      --       TU.suite "prepare" do
      --             TU.testSkip "scalar" do
      --                   let parameters = {date: makeDate 2000 3 4}
      --                   let q = prepare NotNamed parameters $ select (4 # as n) # from (select (4 # as n) # from users # wher (joined .=. datep) # as (Alias :: Alias "u"))
      --                   case Query.query q  of
      --                         NotParameterized s -> TU.failure $ "Expected parameters for " <> s
      --                         Parameterized _ names s p -> do
      --                               TUA.equal "@date" names
      --                               TUA.equal "SELECT 4 AS n FROM (SELECT 4 AS n FROM users WHERE joined = $1) AS u" s
      --                               TUA.equal parameters p
      --                               result q [{n: 4}, {n: 4}]
      --             TU.test "field" do
      --                   let parameters = {date : makeDateTime 2000 3 4 }
      --                   let q = prepare NotNamed parameters $ select id # from (select id # from messages # wher (date .=. datep) # as t)
      --                   case Query.query q  of
      --                         NotParameterized s -> TU.failure $ "Expected parameters for " <> s
      --                         Parameterized _ names s p -> do
      --                               TUA.equal "@date" names
      --                               TUA.equal "SELECT id FROM (SELECT id FROM messages WHERE date = $1) AS t" s
      --                               TUA.equal parameters p
      --                               result q [{id: 1}, {id: 2}]
      --             TU.testSkip "sub query" do
      --                   let parameters = {id :1 }
      --                   let q = prepare NotNamed parameters $ select id # from (select (select id # from messages # wher (idp .=. id)) # from users # wher (id .=. idp) # as t)
      --                   case Query.query q of
      --                         NotParameterized s -> TU.failure $ "Expected parameters for " <> s
      --                         Parameterized _ names s p -> do
      --                               TUA.equal "@id" names
      --                               TUA.equal "SELECT id FROM (SELECT (SELECT id FROM messages WHERE $1 = id) FROM users WHERE id = $1) AS t" s
      --                               TUA.equal parameters p
      --                               result q [{id: 1}, {id: 2}]
      --             TU.testSkip "tuple" do
      --                   let parameters = {id :3 }
      --                   --needs limit
      --                   let q = prepare NotNamed parameters $ select (id /\ date /\ (4 # as n) /\ sent) # from (select (id /\ date /\ (4 # as n) /\ (select sent # from messages # wher (idp .=. idp))) # from messages # wher (idp .=. id) # as t)
      --                   case Query.query q of
      --                         NotParameterized s -> TU.failure $ "Expected parameters for " <> s
      --                         Parameterized _ names s p -> do
      --                               TUA.equal "@id" names
      --                               TUA.equal "SELECT id, date, 4 AS n, sent FROM (SELECT id, date, 4 AS n, (SELECT sent FROM messages WHERE $1 = $1) FROM messages WHERE $1 = id) AS t" s
      --                               TUA.equal parameters p
      --                             --  result q [{id: 1}, {id: 2}]

      -- TU.suite "naked select" do
      --       TU.test "scalar" do
      --             let q = select (3 # as n)
      --             notParameterized "SELECT 3 AS n" $ Query.query q
      --             result q [{n : 3}]
      --       TU.testSkip "sub query" do
      --             let q = select (select (34 # as n) # from users # wher (name .=. name) # as t)
      --             notParameterized "SELECT (SELECT 34 AS n FROM users WHERE name = name) AS t" $ Query.query q
      --             result q [{t : 34}, {t: 34}]
      --       TU.test "tuple" do
      --             --subqueries need to have type Maybe
      --             let q = select ((3 # as b) /\ (select (34 # as n) # from users # wher (name .=. surname) # as t) /\ (4 # as (Alias :: Alias "a")) /\ (select name # from users # as (Alias :: Alias "u")))
      --             notParameterized "SELECT 3 AS b, (SELECT 34 AS n FROM users WHERE name = surname) AS t, 4 AS a, (SELECT name FROM users) AS u" $ Query.query q
      --            -- result q [{b: 3, n: 34, a : 4, name: "mary"}, {b: 3, n: 34, a : 4, name: "mary"}]

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
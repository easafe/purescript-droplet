module Test.Main where

import Droplet.Internal.Edsl.Definition
import Droplet.Internal.Edsl.Filter
import Droplet.Internal.Edsl.Language
import Prelude

import Data.Date (Date)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Internal.Mapper.Driver as Driver
import Droplet.Internal.Mapper.Pool as DIMP
import Droplet.Internal.Mapper.Query (Query(..))
import Droplet.Internal.Mapper.Query as Query
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM

type Users = (id :: Int, name :: String, surname :: String, birthday :: Date, joined :: Date)
type Messages = (id :: Int, name :: String, sent :: Boolean)

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

u :: Alias "u"
u = Alias

b :: Alias "b"
b = Alias

namep :: Parameter "name"
namep = Parameter

main :: Effect Unit
main = TUM.runTest do
      TU.suite "from" do
            TU.test "table" do
                  let q = (select (34 # as b) # from messages)
                  notParameterized "SELECT 34 AS b FROM messages" $ Query.query q
                  result q [{b : 34}, {b : 34}]

      TU.suite "prepare and where" do
            TU.test "equals" do
                  let parameters = {name : "josh"}
                  case Query.query <<< prepare NotNamed parameters $ select id # from users # wher (name .=. namep) of
                        NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                        Parameterized _ names s q -> do
                              TUA.equal "@name" names
                              TUA.equal "SELECT id FROM users WHERE name = $1" s
                              TUA.equal parameters q
            TU.test "not equals" do
                  let parameters = {id: 3}
                  case Query.query <<< prepare NotNamed parameters $ select id # from users # wher (id .<>. (Parameter :: Parameter "id")) of
                        NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                        Parameterized _ names s q -> do
                              TUA.equal "@id" names
                              TUA.equal "SELECT id FROM users WHERE id <> $1" s
                              TUA.equal parameters q

            TU.suite "field" do
                  TU.test "equals" do
                        let q = select (34 # as (Alias :: Alias "n")) # from users # wher (name .=. surname)
                        notParameterized "SELECT 34 AS n FROM users WHERE name = surname" $ Query.query q
                  TU.test "not equals" do
                        let q = select (34 # as (Alias :: Alias "n")) # from users # wher (name .<>. surname)
                        notParameterized "SELECT 34 AS n FROM users WHERE name <> surname" $ Query.query q

            TU.suite "logical operands" do
                  TU.suite "and" do
                        TU.test "single" do
                              let parameters = {name : "josh"}
                              case Query.query <<< prepare NotNamed parameters $ select id # from users # wher (name .=. namep .&&. name .=. surname) of
                                    NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized _ names s q -> do
                                          TUA.equal "@name" names
                                          TUA.equal "SELECT id FROM users WHERE (name = $1 AND name = surname)" s
                                          TUA.equal parameters q
                        TU.test "many" do
                              let parameters = {name : "josh", surname: "j."}
                              case Query.query <<< prepare NotNamed parameters $ select id # from users # wher (name .=. namep .&&. name .=. surname .&&. surname .<>. (Parameter :: Parameter "surname")) of
                                    NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized _ names s q -> do
                                          TUA.equal "@name, @surname" names
                                          TUA.equal "SELECT id FROM users WHERE ((name = $1 AND name = surname) AND surname <> $2)" s
                                          TUA.equal parameters q

                  TU.suite "or" do
                        TU.test "single" do
                              let parameters = {name : "josh"}
                              case Query.query <<< prepare NotNamed parameters $ select id # from users # wher (name .=. namep .||. name .=. surname) of
                                    NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized _ names s q -> do
                                          TUA.equal "@name" names
                                          TUA.equal "SELECT id FROM users WHERE (name = $1 OR name = surname)" s
                                          TUA.equal parameters q

                        TU.test "many" do
                              let parameters = {name : "josh", surname: "j."}
                              case Query.query <<< prepare NotNamed parameters $ select id # from users # wher (name .=. namep .||. name .=. (Parameter :: Parameter "surname") .||. surname .<>. namep) of
                                    NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized _ names s q -> do
                                          TUA.equal "@name, @surname" names
                                          TUA.equal "SELECT id FROM users WHERE ((name = $1 OR name = $2) OR surname <> $1)" s
                                          TUA.equal parameters q

                  TU.suite "tuple" do
                        TU.test "not bracketed" do
                              let parameters = {id3 : 3, id33: 33, id333 : 333}
                              case Query.query <<< prepare NotNamed parameters $ select id # from users # wher (id .=. (Parameter :: Parameter "id333") .||. id .=. (Parameter :: Parameter "id33") .&&. id .=. (Parameter :: Parameter "id3")) of
                                    NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized _ names s q -> do
                                          TUA.equal "@id3, @id33, @id333" names
                                          TUA.equal "SELECT id FROM users WHERE (id = $3 OR (id = $2 AND id = $1))" s
                                          TUA.equal parameters q
                        TU.test "bracketed" do
                              let parameters = {id3 : 3, id33: 33, id333 : 333}
                              case Query.query <<< prepare NotNamed parameters $ select id # from users # wher ((id .=. (Parameter :: Parameter "id3") .||. id .=. (Parameter :: Parameter "id33")) .&&. id .=. (Parameter :: Parameter "id333")) of
                                    NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized _ names s q -> do
                                          TUA.equal "@id3, @id33, @id333" names
                                          TUA.equal "SELECT id FROM users WHERE ((id = $1 OR id = $2) AND id = $3)" s
                                          TUA.equal parameters q

            TU.suite "prepared sub queries" do
                  TU.test "scalar" do
                        let parameters = { d : "4" }
                        case Query.query <<< prepare NotNamed parameters $ select (select (4 # as (Alias :: Alias "n")) # from users # wher (name .=. (Parameter :: Parameter "d")) # as b) of
                              NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized _ names s q -> do
                                    TUA.equal "@d" names
                                    TUA.equal "SELECT (SELECT 4 AS n FROM users WHERE name = $1) AS b" s
                                    TUA.equal parameters q
                  TU.test "field" do
                        let parameters = { d : "4" }
                        case Query.query (prepare NotNamed parameters (select (select id # from users # wher (name .=. (Parameter :: Parameter "d")) # as b))) of
                              NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized _ names s q -> do
                                    TUA.equal "@d" names
                                    TUA.equal "SELECT (SELECT id FROM users WHERE name = $1) AS b" s
                                    TUA.equal parameters q
                  TU.test "tuple" do
                        let parameters = { d : "4" }
                        case Query.query (prepare NotNamed parameters (select ((3 # as (Alias :: Alias "e")) /\ (select id # from users # wher (name .=. (Parameter :: Parameter "d")) # as b) /\ (select name # from users # wher (name .=. (Parameter :: Parameter "d")) # as (Alias :: Alias "c"))))) of
                              NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized _ names s q -> do
                                    TUA.equal "@d" names
                                    TUA.equal "SELECT 3 AS e, (SELECT id FROM users WHERE name = $1) AS b, (SELECT name FROM users WHERE name = $1) AS c" s
                                    TUA.equal parameters q

      TU.suite "as" do
            TU.suite "from" do
                  TU.test "scalar" do
                        let q = select (4 # as (Alias :: Alias "n")) # from (select (4 # as (Alias :: Alias "n")) # from messages # as (Alias :: Alias "u"))
                        notParameterized "SELECT 4 AS n FROM (SELECT 4 AS n FROM messages) AS u" $ Query.query q
                  TU.test "field" do
                        let q = select id # from (select id # from users # as (Alias :: Alias "t"))
                        notParameterized "SELECT id FROM (SELECT id FROM users) AS t" $ Query.query q
                  TU.test "sub Query.query" do
                        let q = select id # from (select (select id # from messages # as (Alias :: Alias "id")) # from users # as (Alias :: Alias "t"))
                        notParameterized "SELECT id FROM (SELECT (SELECT id FROM messages) AS id FROM users) AS t" $ Query.query q
                  TU.test "tuple" do
                        let q = select (id /\ name /\ (4 # as (Alias :: Alias "n")) /\ (Field :: Field "sent")) # from (select (id /\ name /\ (4 # as (Alias :: Alias "n")) /\ (select sent # from messages # as (Alias :: Alias "sent"))) # from messages # as (Alias :: Alias "t"))
                        notParameterized "SELECT id, name, 4 AS n, sent FROM (SELECT id, name, 4 AS n, (SELECT sent FROM messages) AS sent FROM messages) AS t" $ Query.query q

            TU.suite "where" do
                  TU.test "scalar" do
                        let q = select (4 # as (Alias :: Alias "n")) # from (select (4 # as (Alias :: Alias "n")) # from users # wher (id .=. id) # as (Alias :: Alias "u"))
                        notParameterized "SELECT 4 AS n FROM (SELECT 4 AS n FROM users WHERE id = id) AS u" $ Query.query q
                  TU.test "field" do
                        let q = select id # from (select id # from messages # wher (id .=. id) # as (Alias :: Alias "t"))
                        notParameterized "SELECT id FROM (SELECT id FROM messages WHERE id = id) AS t" $ Query.query q
                  TU.test "sub Query.query" do
                        let q = select (Field :: Field "id") # from (select (select id # from messages # wher (id .=. id) # as (Alias :: Alias "id")) # from users # wher (id .=. id) # as (Alias :: Alias "t"))
                        notParameterized "SELECT id FROM (SELECT (SELECT id FROM messages WHERE id = id) AS id FROM users WHERE id = id) AS t" $ Query.query q
                  TU.test "tuple" do
                        let q = select (id /\ name /\ (4 # as (Alias :: Alias "n")) /\ (Field :: Field "sent")) # from (select (id /\ name /\ (4 # as (Alias :: Alias "n")) /\ (select sent # from messages # wher (id .=. id) # as (Alias :: Alias "sent"))) # from messages # wher (id .=. id) # as (Alias :: Alias "t"))
                        notParameterized "SELECT id, name, 4 AS n, sent FROM (SELECT id, name, 4 AS n, (SELECT sent FROM messages WHERE id = id) AS sent FROM messages WHERE id = id) AS t" $ Query.query q

            TU.suite "prepare" do
                  TU.test "scalar" do
                        let parameters = {id :3 }
                        case Query.query <<< prepare NotNamed parameters $ select (4 # as (Alias :: Alias "n")) # from (select (4 # as (Alias :: Alias "n")) # from users # wher (id .=. (Parameter :: Parameter "id")) # as (Alias :: Alias "u")) of
                              NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized _ names s q -> do
                                    TUA.equal "@id" names
                                    TUA.equal "SELECT 4 AS n FROM (SELECT 4 AS n FROM users WHERE id = $1) AS u" s
                                    TUA.equal parameters q
                  TU.test "field" do
                        let parameters = {id :3 }
                        case Query.query <<< prepare NotNamed parameters $ select id # from (select id # from messages # wher (id .=. (Parameter :: Parameter "id")) # as (Alias :: Alias "t")) of
                              NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized _ names s q -> do
                                    TUA.equal "@id" names
                                    TUA.equal "SELECT id FROM (SELECT id FROM messages WHERE id = $1) AS t" s
                                    TUA.equal parameters q
                  TU.test "sub Query.query" do
                        let parameters = {id :3 }
                        case Query.query <<< prepare NotNamed parameters $ select (Field :: Field "id") # from (select (select id # from messages # wher ((Parameter :: Parameter "id") .=. id) # as (Alias :: Alias "id")) # from users # wher (id .=. (Parameter :: Parameter "id")) # as (Alias :: Alias "t")) of
                              NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized _ names s q -> do
                                    TUA.equal "@id" names
                                    TUA.equal "SELECT id FROM (SELECT (SELECT id FROM messages WHERE $1 = id) AS id FROM users WHERE id = $1) AS t" s
                                    TUA.equal parameters q
                  TU.test "tuple" do
                        let parameters = {id :3 }
                        case Query.query <<< prepare NotNamed parameters $ select (id /\ name /\ (4 # as (Alias :: Alias "n")) /\ (Field :: Field "sent")) # from (select (id /\ name /\ (4 # as (Alias :: Alias "n")) /\ (select sent # from messages # wher ((Parameter :: Parameter "id") .=. (Parameter :: Parameter "id")) # as (Alias :: Alias "sent"))) # from messages # wher ((Parameter :: Parameter "id") .=. id) # as (Alias :: Alias "t")) of
                              NotParameterized s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized _ names s q -> do
                                    TUA.equal "@id" names
                                    TUA.equal "SELECT id, name, 4 AS n, sent FROM (SELECT id, name, 4 AS n, (SELECT sent FROM messages WHERE $1 = $1) AS sent FROM messages WHERE $1 = id) AS t" s
                                    TUA.equal parameters q

      TU.suite "naked select" do
            TU.test "scalar" do
                  let q = select (3 # as (Alias :: Alias "n"))
                  notParameterized "SELECT 3 AS n" $ Query.query q
            TU.test "sub Query.query" do
                  let q = select (select (34 # as (Alias :: Alias "n")) # from users # wher (name .=. surname) # as (Alias :: Alias "t"))
                  notParameterized "SELECT (SELECT 34 AS n FROM users WHERE name = surname) AS t" $ Query.query q
            TU.test "tuple" do
                  let q = select ((3 # as b) /\ (select (34 # as (Alias :: Alias "n")) # from users # wher (name .=. surname) # as (Alias :: Alias "t")) /\ (4 # as (Alias :: Alias "a")) /\ (select name # from users # as (Alias :: Alias "u")))
                  notParameterized "SELECT 3 AS b, (SELECT 34 AS n FROM users WHERE name = surname) AS t, 4 AS a, (SELECT name FROM users) AS u" $ Query.query q

notParameterized :: forall projection p. String -> Query projection p -> _
notParameterized s q = case q of
      NotParameterized s' -> TUA.equal s s'
      _ -> TU.failure $ "Expected no parameters for " <> s

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
module Test.Main where

import Droplet
import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM

type Users = (id :: Int, name :: String, surname :: String, birthday :: Date, joined :: Date)

users :: Table "users" Users
users = Table

u :: Alias "u"
u = Alias

id :: Field "id"
id = Field

name :: Field "name"
name = Field

surname :: Field "surname"
surname = Field

sent :: Field "sent"
sent = Field

type Messages = (id :: Int, name :: String, sent :: Boolean)

messages :: Table "messages" Messages
messages = Table

main :: Effect Unit
main = TUM.runTest do
      TU.suite "select" do
            TU.suite "single column" do
                  TU.test "scalar" do
                        let q = query $ select 3
                        plain "SELECT 3" q
                  TU.test "field" do
                        let q = query $ select id
                        plain "SELECT id" q
                  TU.test "star" do
                        let q = query $ select star
                        plain "SELECT *" q

            TU.suite "columns" do
                  TU.test "scalars" do
                        let q = query $ select (3 /\ 4 /\ 5)
                        plain "SELECT 3, 4, 5" q
                  TU.test "fields" do
                        let q = query $ select (id /\ name)
                        plain "SELECT id, name" q
                  TU.test "tuple" do
                        let q = query $ select (star /\ id /\ 5)
                        plain "SELECT *, id, 5" q

            TU.suite "sub q" do
                  TU.test "scalars" do
                        let q = query $ select (3 /\ (select 34 # from users # wher (name .=. surname)))
                        plain "SELECT 3, (SELECT 34 FROM users WHERE name = surname)" q
                  TU.test "fields" do
                        let q = query $ select (id /\ (select (Field :: Field "sent") # from messages))
                        plain "SELECT id, (SELECT sent FROM messages)" q
                  TU.test "tuple" do
                        let q = query $ select (3 /\ (select id # from messages) /\ id /\ name /\ star /\ (select (Field :: Field "sent") # from messages) /\ (select 5 # from messages))
                        plain "SELECT 3, (SELECT id FROM messages), id, name, *, (SELECT sent FROM messages), (SELECT 5 FROM messages)" q

      TU.suite "from" do
            TU.test "table" do
                  let q = query $ select 34 # from messages
                  plain "SELECT 34 FROM messages" q

      TU.suite "prepare and where" do
            TU.test "equals" do
                  let parameters = {name : "josh"}
                  case query <<< prepare parameters $ select id # from users # wher (name .=. (Parameter :: Parameter "name")) of
                        Plain s -> TU.failure $ "Expected parameters for " <> s
                        Parameterized s q -> do
                              TUA.equal "SELECT id FROM users WHERE name = @name" s
                              TUA.equal parameters q
            TU.test "not equals" do
                  let parameters = {id: 3}
                  case query <<< prepare parameters $ select id # from users # wher (id .<>. (Parameter :: Parameter "id")) of
                        Plain s -> TU.failure $ "Expected parameters for " <> s
                        Parameterized s q -> do
                              TUA.equal "SELECT id FROM users WHERE id <> @id" s
                              TUA.equal parameters q

            TU.suite "field" do
                  TU.test "equals" do
                        let q = query $ select 34 # from users # wher (name .=. surname)
                        plain "SELECT 34 FROM users WHERE name = surname" q
                  TU.test "not equals" do
                        let q = query $ select 34 # from users # wher (name .<>. surname)
                        plain "SELECT 34 FROM users WHERE name <> surname" q

            TU.suite "logical operands" do
                  TU.suite "and" do
                        TU.test "single" do
                              let parameters = {name : "josh"}
                              case query <<< prepare parameters $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .&&. name .=. surname) of
                                    Plain s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized s q -> do
                                          TUA.equal "SELECT id FROM users WHERE (name = @name AND name = surname)" s
                                          TUA.equal parameters q
                        TU.test "many" do
                              let parameters = {name : "josh", surname: "j."}
                              case query <<< prepare parameters $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .&&. name .=. surname .&&. surname .<>. (Parameter :: Parameter "surname")) of
                                    Plain s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized s q -> do
                                          TUA.equal "SELECT id FROM users WHERE ((name = @name AND name = surname) AND surname <> @surname)" s
                                          TUA.equal parameters q

                  TU.suite "or" do
                        TU.test "single" do
                              let parameters = {name : "josh"}
                              case query <<< prepare parameters $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .||. name .=. surname) of
                                    Plain s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized s q -> do
                                          TUA.equal "SELECT id FROM users WHERE (name = @name OR name = surname)" s
                                          TUA.equal parameters q

                        TU.test "many" do
                              let parameters = {name : "josh", surname: "j."}
                              case query <<< prepare parameters $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .||. name .=. surname .||. surname .<>. (Parameter :: Parameter "surname")) of
                                    Plain s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized s q -> do
                                          TUA.equal "SELECT id FROM users WHERE ((name = @name OR name = surname) OR surname <> @surname)" s
                                          TUA.equal parameters q

                  TU.suite "tuple" do
                        TU.test "not bracketed" do
                              let parameters = {id3 : 3, id33: 33, id333 : 333}
                              case query <<< prepare parameters $ select id # from users # wher (id .=. (Parameter :: Parameter "id3") .||. id .=. (Parameter :: Parameter "id33") .&&. id .=. (Parameter :: Parameter "id333")) of
                                    Plain s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized s q -> do
                                          TUA.equal "SELECT id FROM users WHERE (id = @id3 OR (id = @id33 AND id = @id333))" s
                                          TUA.equal parameters q
                        TU.test "bracketed" do
                              let parameters = {id3 : 3, id33: 33, id333 : 333}
                              case query <<< prepare parameters $ select id # from users # wher ((id .=. (Parameter :: Parameter "id3") .||. id .=. (Parameter :: Parameter "id33")) .&&. id .=. (Parameter :: Parameter "id333")) of
                                    Plain s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized s q -> do
                                          TUA.equal "SELECT id FROM users WHERE ((id = @id3 OR id = @id33) AND id = @id333)" s
                                          TUA.equal parameters q

            TU.suite "prepared sub queries" do
                  TU.test "scalar" do
                        let parameters = { d : "4" }
                        case query <<< prepare parameters $ select (select 4 # from users # wher (name .=. (Parameter :: Parameter "d"))) of
                              Plain s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized s q -> do
                                    TUA.equal "SELECT (SELECT 4 FROM users WHERE name = @d)" s
                                    TUA.equal parameters q
                  TU.test "field" do
                        let parameters = { d : "4" }
                        case query <<< prepare parameters $ select (select id # from users # wher (name .=. (Parameter :: Parameter "d"))) of
                              Plain s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized s q -> do
                                    TUA.equal "SELECT (SELECT id FROM users WHERE name = @d)" s
                                    TUA.equal parameters q
                  TU.test "tuple" do
                        let parameters = { d : "4" }
                        case query <<< prepare parameters $ select (3 /\ name /\ (select id # from users # wher (name .=. (Parameter :: Parameter "d"))) /\ (select name # from users # wher (name .=. (Parameter :: Parameter "d")))) of
                              Plain s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized s q -> do
                                    TUA.equal "SELECT 3, name, (SELECT id FROM users WHERE name = @d), (SELECT name FROM users WHERE name = @d)" s
                                    TUA.equal parameters q

      TU.suite "as" do
            TU.suite "select" do
                  TU.test "scalar" do
                        let q = query $ select 4 # from (select 4 # as (Alias :: Alias "u"))
                        plain "SELECT 4 FROM (SELECT 4) AS u" q
                  TU.test "field" do
                        let q = query $ select id # from (select id # as (Alias :: Alias "t"))
                        plain "SELECT id FROM (SELECT id) AS t" q
                  TU.test "sub query" do
                        let q = query $ select id # from (select (select id # from messages) # as (Alias :: Alias "t"))
                        plain "SELECT id FROM (SELECT (SELECT id FROM messages)) AS t" q
                  TU.test "tuple" do
                        let q = query $ select star # from (select (3 /\ 5 /\ (select id # from messages)) # as (Alias :: Alias "t"))
                        plain "SELECT * FROM (SELECT 3, 5, (SELECT id FROM messages)) AS t" q

            TU.suite "from" do
                  TU.test "scalar" do
                        let q = query $ select 4 # from (select 4 # from users # as (Alias :: Alias "u"))
                        plain "SELECT 4 FROM (SELECT 4 FROM users) AS u" q
                  TU.test "field" do
                        let q = query $ select id # from (select id # from messages # as (Alias :: Alias "t"))
                        plain "SELECT id FROM (SELECT id FROM messages) AS t" q
                  TU.test "sub query" do
                        let q = query $ select id # from (select (select id # from messages) # from users # as (Alias :: Alias "t"))
                        plain "SELECT id FROM (SELECT (SELECT id FROM messages) FROM users) AS t" q
                  TU.test "tuple" do
                        let q = query $ select (id /\ name /\ 4 /\ sent) # from (select (id /\ name /\ 4 /\ (select sent # from messages)) # from messages # as (Alias :: Alias "t"))
                        plain "SELECT id, name, 4, sent FROM (SELECT id, name, 4, (SELECT sent FROM messages) FROM messages) AS t" q

            TU.suite "where" do
                  TU.test "scalar" do
                        let q = query $ select 4 # from (select 4 # from users # wher (id .=. id) # as (Alias :: Alias "u"))
                        plain "SELECT 4 FROM (SELECT 4 FROM users WHERE id = id) AS u" q
                  TU.test "field" do
                        let q = query $ select id # from (select id # from messages # wher (id .=. id) # as (Alias :: Alias "t"))
                        plain "SELECT id FROM (SELECT id FROM messages WHERE id = id) AS t" q
                  TU.test "sub query" do
                        let q = query $ select id # from (select (select id # from messages # wher (id .=. id)) # from users # wher (id .=. id) # as (Alias :: Alias "t"))
                        plain "SELECT id FROM (SELECT (SELECT id FROM messages WHERE id = id) FROM users WHERE id = id) AS t" q
                  TU.test "tuple" do
                        let q = query $ select (id /\ name /\ 4 /\ sent) # from (select (id /\ name /\ 4 /\ (select sent # from messages # wher (id .=. id))) # from messages # wher (id .=. id) # as (Alias :: Alias "t"))
                        plain "SELECT id, name, 4, sent FROM (SELECT id, name, 4, (SELECT sent FROM messages WHERE id = id) FROM messages WHERE id = id) AS t" q

            TU.suite "prepare" do
                  TU.test "scalar" do
                        let parameters = {id :3 }
                        case query <<< prepare parameters $ select 4 # from (select 4 # from users # wher (id .=. (Parameter :: Parameter "id")) # as (Alias :: Alias "u")) of
                              Plain s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized s q -> do
                                    TUA.equal "SELECT 4 FROM (SELECT 4 FROM users WHERE id = @id) AS u" s
                                    TUA.equal parameters q
                  TU.test "field" do
                        let parameters = {id :3 }
                        case query <<< prepare parameters $ select id # from (select id # from messages # wher (id .=. (Parameter :: Parameter "id")) # as (Alias :: Alias "t")) of
                              Plain s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized s q -> do
                                    TUA.equal "SELECT id FROM (SELECT id FROM messages WHERE id = @id) AS t" s
                                    TUA.equal parameters q
                  TU.test "sub query" do
                        let parameters = {id :3 }
                        case query <<< prepare parameters $ select id # from (select (select id # from messages # wher ((Parameter :: Parameter "id") .=. id)) # from users # wher (id .=. (Parameter :: Parameter "id")) # as (Alias :: Alias "t")) of
                              Plain s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized s q -> do
                                    TUA.equal "SELECT id FROM (SELECT (SELECT id FROM messages WHERE @id = id) FROM users WHERE id = @id) AS t" s
                                    TUA.equal parameters q
                  TU.test "tuple" do
                        let parameters = {id :3 }
                        case query <<< prepare parameters $ select (id /\ name /\ 4 /\ sent) # from (select (id /\ name /\ 4 /\ (select sent # from messages # wher ((Parameter :: Parameter "id") .=. (Parameter :: Parameter "id")))) # from messages # wher ((Parameter :: Parameter "id") .=. id) # as (Alias :: Alias "t")) of
                              Plain s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized s q -> do
                                    TUA.equal "SELECT id, name, 4, sent FROM (SELECT id, name, 4, (SELECT sent FROM messages WHERE @id = @id) FROM messages WHERE @id = id) AS t" s
                                    TUA.equal parameters q

plain :: forall p. String -> Query p -> _
plain s q = case q of
      Plain s' -> TUA.equal s s'
      _ -> TU.failure $ "Expected no parameters for " <> s


-- parameterized :: forall p. String -> Record p -> Query p -> _
-- parameterized query parameters q = case q of
--       Parameterized s p -> do
--             TUA.equal query s
--             TUA.equal parameters p
--       _ -> TU.failure $ "Expected parameters for " <> query



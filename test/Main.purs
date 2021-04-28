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

--test other fields of Parameterized once they are set again
main :: Effect Unit
main = TUM.runTest do
      TU.suite "from" do
            TU.test "table" do
                  let q = query $ select 34 # from messages
                  plain "SELECT 34 FROM messages" q

      TU.suite "prepare and where" do
            TU.test "equals" do
                  let parameters = {name : "josh"}
                  case query <<< prepare parameters $ select id # from users # wher (name .=. (Parameter :: Parameter "name")) of
                        Plain s -> TU.failure $ "Expected parameters for " <> s
                        Parameterized _ names s q -> do
                              TUA.equal (Just "@name") names
                              TUA.equal "SELECT id FROM users WHERE name = $1" s
                              TUA.equal parameters q
            TU.test "not equals" do
                  let parameters = {id: 3}
                  case query <<< prepare parameters $ select id # from users # wher (id .<>. (Parameter :: Parameter "id")) of
                        Plain s -> TU.failure $ "Expected parameters for " <> s
                        Parameterized _ names s q -> do
                              TUA.equal (Just "@id") names
                              TUA.equal "SELECT id FROM users WHERE id <> $1" s
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
                                    Parameterized _ names s q -> do
                                          TUA.equal (Just "@name") names
                                          TUA.equal "SELECT id FROM users WHERE (name = $1 AND name = surname)" s
                                          TUA.equal parameters q
                        TU.test "many" do
                              let parameters = {name : "josh", surname: "j."}
                              case query <<< prepare parameters $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .&&. name .=. surname .&&. surname .<>. (Parameter :: Parameter "surname")) of
                                    Plain s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized _ names s q -> do
                                          TUA.equal (Just "@name, @surname") names
                                          TUA.equal "SELECT id FROM users WHERE ((name = $1 AND name = surname) AND surname <> $2)" s
                                          TUA.equal parameters q

                  TU.suite "or" do
                        TU.test "single" do
                              let parameters = {name : "josh"}
                              case query <<< prepare parameters $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .||. name .=. surname) of
                                    Plain s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized _ names s q -> do
                                          TUA.equal (Just "@name") names
                                          TUA.equal "SELECT id FROM users WHERE (name = $1 OR name = surname)" s
                                          TUA.equal parameters q

                        TU.test "many" do
                              let parameters = {name : "josh", surname: "j."}
                              case query <<< prepare parameters $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .||. name .=. (Parameter :: Parameter "surname") .||. surname .<>. (Parameter :: Parameter "name")) of
                                    Plain s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized _ names s q -> do
                                          TUA.equal (Just "@name, @surname") names
                                          TUA.equal "SELECT id FROM users WHERE ((name = $1 OR name = $2) OR surname <> $1)" s
                                          TUA.equal parameters q

                  TU.suite "tuple" do
                        TU.test "not bracketed" do
                              let parameters = {id3 : 3, id33: 33, id333 : 333}
                              case query <<< prepare parameters $ select id # from users # wher (id .=. (Parameter :: Parameter "id333") .||. id .=. (Parameter :: Parameter "id33") .&&. id .=. (Parameter :: Parameter "id3")) of
                                    Plain s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized _ names s q -> do
                                          TUA.equal (Just "@id3, @id33, @id333") names
                                          TUA.equal "SELECT id FROM users WHERE (id = $3 OR (id = $2 AND id = $1))" s
                                          TUA.equal parameters q
                        TU.test "bracketed" do
                              let parameters = {id3 : 3, id33: 33, id333 : 333}
                              case query <<< prepare parameters $ select id # from users # wher ((id .=. (Parameter :: Parameter "id3") .||. id .=. (Parameter :: Parameter "id33")) .&&. id .=. (Parameter :: Parameter "id333")) of
                                    Plain s -> TU.failure $ "Expected parameters for " <> s
                                    Parameterized _ names s q -> do
                                          TUA.equal (Just "@id3, @id33, @id333") names
                                          TUA.equal "SELECT id FROM users WHERE ((id = $1 OR id = $2) AND id = $3)" s
                                          TUA.equal parameters q

            TU.suite "prepared sub queries" do
                  TU.test "scalar" do
                        let parameters = { d : "4" }
                        case query <<< prepare parameters $ select (select 4 # from users # wher (name .=. (Parameter :: Parameter "d")) # as (Alias :: Alias "b")) of
                              Plain s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized _ names s q -> do
                                    TUA.equal (Just "@d") names
                                    TUA.equal "SELECT (SELECT 4 FROM users WHERE name = $1) AS b" s
                                    TUA.equal parameters q
                  TU.test "field" do
                        let parameters = { d : "4" }
                        case query <<< prepare parameters $ select (select id # from users # wher (name .=. (Parameter :: Parameter "d")) # as (Alias :: Alias "b")) of
                              Plain s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized _ names s q -> do
                                    TUA.equal (Just "@d") names
                                    TUA.equal "SELECT (SELECT id FROM users WHERE name = $1) AS b" s
                                    TUA.equal parameters q
                  TU.test "tuple" do
                        let parameters = { d : "4" }
                        case query <<< prepare parameters $ select (3 /\ (select id # from users # wher (name .=. (Parameter :: Parameter "d")) # as (Alias :: Alias "b")) /\ (select name # from users # wher (name .=. (Parameter :: Parameter "d")) # as (Alias :: Alias "c"))) of
                              Plain s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized _ names s q -> do
                                    TUA.equal (Just "@d") names
                                    TUA.equal "SELECT 3, (SELECT id FROM users WHERE name = $1) AS b, (SELECT name FROM users WHERE name = $1) AS c" s
                                    TUA.equal parameters q

      TU.suite "as" do
            TU.suite "from" do
                  TU.test "scalar" do
                        let q = query $ select 4 # from (select 4 # from messages # as (Alias :: Alias "u"))
                        plain "SELECT 4 FROM (SELECT 4 FROM messages) AS u" q
                  TU.test "field" do
                        let q = query $ select id # from (select id # from users # as (Alias :: Alias "t"))
                        plain "SELECT id FROM (SELECT id FROM users) AS t" q
                  TU.test "sub query" do
                        let q = query $ select id # from (select (select id # from messages # as (Alias :: Alias "id")) # from users # as (Alias :: Alias "t"))
                        plain "SELECT id FROM (SELECT (SELECT id FROM messages) AS id FROM users) AS t" q
                  TU.test "tuple" do
                        let q = query $ select (id /\ name /\ 4 /\ (Field :: Field "sent")) # from (select (id /\ name /\ 4 /\ (select sent # from messages # as (Alias :: Alias "sent"))) # from messages # as (Alias :: Alias "t"))
                        plain "SELECT id, name, 4, sent FROM (SELECT id, name, 4, (SELECT sent FROM messages) AS sent FROM messages) AS t" q

            TU.suite "where" do
                  TU.test "scalar" do
                        let q = query $ select 4 # from (select 4 # from users # wher (id .=. id) # as (Alias :: Alias "u"))
                        plain "SELECT 4 FROM (SELECT 4 FROM users WHERE id = id) AS u" q
                  TU.test "field" do
                        let q = query $ select id # from (select id # from messages # wher (id .=. id) # as (Alias :: Alias "t"))
                        plain "SELECT id FROM (SELECT id FROM messages WHERE id = id) AS t" q
                  TU.test "sub query" do
                        let q = query $ select (Field :: Field "id") # from (select (select id # from messages # wher (id .=. id) # as (Alias :: Alias "id")) # from users # wher (id .=. id) # as (Alias :: Alias "t"))
                        plain "SELECT id FROM (SELECT (SELECT id FROM messages WHERE id = id) AS id FROM users WHERE id = id) AS t" q
                  TU.test "tuple" do
                        let q = query $ select (id /\ name /\ 4 /\ (Field :: Field "sent")) # from (select (id /\ name /\ 4 /\ (select sent # from messages # wher (id .=. id) # as (Alias :: Alias "sent"))) # from messages # wher (id .=. id) # as (Alias :: Alias "t"))
                        plain "SELECT id, name, 4, sent FROM (SELECT id, name, 4, (SELECT sent FROM messages WHERE id = id) AS sent FROM messages WHERE id = id) AS t" q

            TU.suite "prepare" do
                  TU.test "scalar" do
                        let parameters = {id :3 }
                        case query <<< prepare parameters $ select 4 # from (select 4 # from users # wher (id .=. (Parameter :: Parameter "id")) # as (Alias :: Alias "u")) of
                              Plain s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized _ names s q -> do
                                    TUA.equal (Just "@id") names
                                    TUA.equal "SELECT 4 FROM (SELECT 4 FROM users WHERE id = $1) AS u" s
                                    TUA.equal parameters q
                  TU.test "field" do
                        let parameters = {id :3 }
                        case query <<< prepare parameters $ select id # from (select id # from messages # wher (id .=. (Parameter :: Parameter "id")) # as (Alias :: Alias "t")) of
                              Plain s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized _ names s q -> do
                                    TUA.equal (Just "@id") names
                                    TUA.equal "SELECT id FROM (SELECT id FROM messages WHERE id = $1) AS t" s
                                    TUA.equal parameters q
                  TU.test "sub query" do
                        let parameters = {id :3 }
                        case query <<< prepare parameters $ select (Field :: Field "id") # from (select (select id # from messages # wher ((Parameter :: Parameter "id") .=. id) # as (Alias :: Alias "id")) # from users # wher (id .=. (Parameter :: Parameter "id")) # as (Alias :: Alias "t")) of
                              Plain s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized _ names s q -> do
                                    TUA.equal (Just "@id") names
                                    TUA.equal "SELECT id FROM (SELECT (SELECT id FROM messages WHERE $1 = id) AS id FROM users WHERE id = $1) AS t" s
                                    TUA.equal parameters q
                  TU.test "tuple" do
                        let parameters = {id :3 }
                        case query <<< prepare parameters $ select (id /\ name /\ 4 /\ (Field :: Field "sent")) # from (select (id /\ name /\ 4 /\ (select sent # from messages # wher ((Parameter :: Parameter "id") .=. (Parameter :: Parameter "id")) # as (Alias :: Alias "sent"))) # from messages # wher ((Parameter :: Parameter "id") .=. id) # as (Alias :: Alias "t")) of
                              Plain s -> TU.failure $ "Expected parameters for " <> s
                              Parameterized _ names s q -> do
                                    TUA.equal (Just "@id") names
                                    TUA.equal "SELECT id, name, 4, sent FROM (SELECT id, name, 4, (SELECT sent FROM messages WHERE $1 = $1) AS sent FROM messages WHERE $1 = id) AS t" s
                                    TUA.equal parameters q

      TU.suite "naked select" do
            TU.test "scalar" do
                  let q = query $ select 3
                  plain "SELECT 3" q
            TU.test "sub query" do
                  let q = query $ select (select 34 # from users # wher (name .=. surname) # as (Alias :: Alias "t"))
                  plain "SELECT (SELECT 34 FROM users WHERE name = surname) AS t" q
            TU.test "tuple" do
                  let q = query $ select (3 /\ (select 34 # from users # wher (name .=. surname) # as (Alias :: Alias "t")) /\ 4 /\ (select name # from users # as (Alias :: Alias "u")))
                  plain "SELECT 3, (SELECT 34 FROM users WHERE name = surname) AS t, 4, (SELECT name FROM users) AS u" q

plain :: forall p. String -> Query p -> _
plain s q = case q of
      Plain s' -> TUA.equal s s'
      _ -> TU.failure $ "Expected no parameters for " <> s

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
                  TU.test "mixed" do
                        let q = query $ select (star /\ id /\ 5)
                        plain "SELECT *, id, 5" q

            TU.suite "sub q" do
                  TU.test "scalars" do
                        let q = query $ select (3 /\ (select 34 # from users # wher (name .=. surname)))
                        plain "SELECT 3, (SELECT 34 FROM users WHERE name = surname)" q
                  TU.test "fields" do
                        let q = query $ select (id /\ (select (Field :: Field "sent") # from messages))
                        plain "SELECT id, (SELECT sent FROM messages)" q
                  TU.test "mixed" do
                        let q = query $ select (3 /\ (select id # from messages) /\ id /\ name /\ star /\ (select (Field :: Field "sent") # from messages) /\ (select 5 # from messages))
                        plain "SELECT 3, (SELECT id FROM messages), id, name, *, (SELECT sent FROM messages), (SELECT 5 FROM messages)" q

      TU.suite "from" do
            TU.test "table" do
                  let q = query $ select 34 # from messages
                  plain "SELECT 34 FROM messages" q

      TU.suite "where" do
            TU.suite "field" do
                  TU.test "equals" do
                        let q = query $ select 34 # from users # wher (name .=. surname)
                        plain "SELECT 34 FROM users WHERE name = surname" q
                  TU.test "not equals" do
                        let q = query $ select 34 # from users # wher (name .<>. surname)
                        plain "SELECT 34 FROM users WHERE name <> surname" q

            TU.suite "parameters" do
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

            -- TU.suite "logical operands" do
            --       TU.suite "and" do
            --             TU.test "single" do
            --                   let parameters = {name : "josh"}
            --                   let Query q p = query $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .&&. name .=. surname) parameters
            --                   TUA.equal "SELECT id FROM users WHERE (name = @name AND name = surname)" q
            --                   TUA.equal (Just parameters) p
            --             TU.test "many" do
            --                   let parameters = {name : "josh", surname: "j."}
            --                   let Query q p = query $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .&&. name .=. surname .&&. surname .<>. (Parameter :: Parameter "surname")) parameters
            --                   TUA.equal "SELECT id FROM users WHERE ((name = @name AND name = surname) AND surname <> @surname)" q
            --                   TUA.equal (Just parameters) p

            --       TU.suite "or" do
            --             TU.test "single" do
            --                   let parameters = {name : "josh"}
            --                   let Query q p = query $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .||. name .=. surname) parameters
            --                   TUA.equal "SELECT id FROM users WHERE (name = @name OR name = surname)" q
            --                   TUA.equal (Just parameters) p
            --             TU.test "many" do
            --                   let parameters = {name : "josh", surname: "j."}
            --                   let Query q p = query $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .||. name .=. surname .||. surname .<>. (Parameter :: Parameter "surname")) parameters
            --                   TUA.equal "SELECT id FROM users WHERE ((name = @name OR name = surname) OR surname <> @surname)" q
            --                   TUA.equal (Just parameters) p

            --       TU.suite "mixed" do
            --             TU.test "not bracketed" do
            --                   let parameters = {id3 : 3, id33: 33, id333 : 333}
            --                   let Query q p = query $ select id # from users # wher (id .=. (Parameter :: Parameter "id3") .||. id .=. (Parameter :: Parameter "id33") .&&. id .=. (Parameter :: Parameter "id333")) parameters
            --                   TUA.equal "SELECT id FROM users WHERE (id = @id3 OR (id = @id33 AND id = @id333))" q
            --                   TUA.equal (Just parameters) p
            --             TU.test "bracketed" do
            --                   let parameters = {id3 : 3, id33: 33, id333 : 333}
            --                   let Query q p = query $ select id # from users # wher ((id .=. (Parameter :: Parameter "id3") .||. id .=. (Parameter :: Parameter "id33")) .&&. id .=. (Parameter :: Parameter "id333")) parameters
            --                   TUA.equal "SELECT id FROM users WHERE ((id = @id3 OR id = @id33) AND id = @id333)" q
            --                   TUA.equal (Just parameters) p

      -- TU.suite "as" do
      --       TU.suite "named sub queries" do
      --             TU.test "scalars" do
      --                   let q = query $ select 3 # from (select 34 # from users # as u)
      --                   plain "SELECT 3 FROM (SELECT 34 FROM users) u" q
      --             TU.test "fields" do
      --                   let q = query $ select id # from (select (id /\ name) # from users # as u)
      --                   plain "SELECT id FROM (SELECT id, name FROM users) u" q
      --             TU.test "table" do
      --                   let q = query $ select star # from (select star # from users # as u)
      --                   plain "SELECT * FROM (SELECT * FROM users) u" q
      --             TU.test "filtered" do
      --                   let q = query $ select star # from (select (id /\ name /\ surname) # from users # wher (name .=. (Parameter :: Parameter "n")) {n : "nn"} # as u)
      --                   plain "SELECT * FROM (SELECT id, name, surname FROM users WHERE name = @n) u" q

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



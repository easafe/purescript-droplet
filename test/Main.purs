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
                        let query = toQuery $ select 3
                        noParameters "SELECT 3" query
                  TU.test "field" do
                        let query = toQuery $ select id
                        noParameters "SELECT id" query
                  TU.test "table" do
                        let query = toQuery $ select users
                        noParameters "SELECT users.*" query

            TU.suite "columns" do
                  TU.test "scalars" do
                        let query = toQuery $ select (3 /\ 4 /\ 5)
                        noParameters "SELECT 3, 4, 5" query
                  TU.test "fields" do
                        let query = toQuery $ select (id /\ name)
                        noParameters "SELECT id, name" query
                  TU.test "mixed" do
                        let query = toQuery $ select (users /\ id /\ 5)
                        noParameters "SELECT users.*, id, 5" query

            TU.suite "sub query" do
                  TU.test "scalars" do
                        let query = toQuery $ select (3 /\ (select 34 # from users # wher (name .=. surname) {}))
                        noParameters "SELECT 3, (SELECT 34 FROM users WHERE name = surname)" query
                  TU.test "fields" do
                        let query = toQuery $ select (id /\ (select (Field :: Field "sent") # from messages))
                        noParameters "SELECT id, (SELECT sent FROM messages)" query
                  TU.test "mixed" do
                        let query = toQuery $ select (3 /\ (select id # from messages) /\ id /\ name /\ users /\ (select (Field :: Field "sent") # from messages) /\ (select 5 # from messages))
                        noParameters "SELECT 3, (SELECT id FROM messages), id, name, users.*, (SELECT sent FROM messages), (SELECT 5 FROM messages)" query

      TU.suite "from" do
            TU.test "table" do
                  let query = toQuery $ select 34 # from messages
                  noParameters "SELECT 34 FROM messages" query

      TU.suite "where" do
            TU.suite "field" do
                  TU.test "equals" do
                        let query = toQuery $ select 34 # from users # wher (name .=. surname) {}
                        withParameters "SELECT 34 FROM users WHERE name = surname" query
                  TU.test "not equals" do
                        let query = toQuery $ select 34 # from users # wher (name .<>. surname) {}
                        withParameters "SELECT 34 FROM users WHERE name <> surname" query

            TU.suite "parameters" do
                  TU.test "equals" do
                        let query = toQuery $ select id # from users # wher (name .=. (Parameter :: Parameter "name")) {name : "josh"}
                        withParameters "SELECT id FROM users WHERE name = @name" query
                  TU.test "not equals" do
                        let query = toQuery $ select id # from users # wher (id .<>. (Parameter :: Parameter "id")) {id: 3}
                        withParameters "SELECT id FROM users WHERE id <> @id" query

            TU.suite "logical operands" do
                  TU.suite "and" do
                        TU.test "single" do
                              let query = toQuery $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .&&. name .=. surname) {name : "josh"}
                              withParameters "SELECT id FROM users WHERE (name = @name AND name = surname)" query
                        TU.test "many" do
                              let query = toQuery $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .&&. name .=. surname .&&. surname .<>. (Parameter :: Parameter "surname")) {name : "josh", surname: "j."}
                              withParameters "SELECT id FROM users WHERE ((name = @name AND name = surname) AND surname <> @surname)" query

                  TU.suite "or" do
                        TU.test "single" do
                              let query = toQuery $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .||. name .=. surname) {name : "josh"}
                              withParameters "SELECT id FROM users WHERE (name = @name OR name = surname)" query
                        TU.test "many" do
                              let query = toQuery $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .||. name .=. surname .||. surname .<>. (Parameter :: Parameter "surname")) {name : "josh", surname: "j."}
                              withParameters "SELECT id FROM users WHERE ((name = @name OR name = surname) OR surname <> @surname)" query

                  TU.suite "mixed" do
                        TU.test "not bracketed" do
                              let query = toQuery $ select id # from users # wher (id .=. (Parameter :: Parameter "id3") .||. id .=. (Parameter :: Parameter "id33") .&&. id .=. (Parameter :: Parameter "id333")) {id3 : 3, id33: 33, id333 : 333}
                              withParameters "SELECT id FROM users WHERE (id = @id3 OR (id = @id33 AND id = @id333))" query
                        TU.test "bracketed" do
                              let query = toQuery $ select id # from users # wher ((id .=. (Parameter :: Parameter "id3") .||. id .=. (Parameter :: Parameter "id33")) .&&. id .=. (Parameter :: Parameter "id333")) {id3 : 3, id33: 33, id333 : 333}
                              withParameters "SELECT id FROM users WHERE ((id = @id3 OR id = @id33) AND id = @id333)" query

      TU.suite "as" do
            TU.suite "named sub queries" do
                  TU.test "scalars" do
                        let query = toQuery $ select 3 # from (select 34 # from users # as u)
                        noParameters "SELECT 3 FROM (SELECT 34 FROM users) u" query
                  TU.test "fields" do
                        let query = toQuery $ select id # from (select (id /\ name) # from users # as u)
                        noParameters "SELECT id FROM (SELECT id, name FROM users) u" query
                  TU.test "table" do
                        let query = toQuery $ select users # from (select users # from users # as u)
                        noParameters "SELECT u.* FROM (SELECT users.* FROM users) u" query

noParameters :: forall p. String -> Query p -> _
noParameters s (Query q p) = case p of
      Nothing -> TUA.equal s q
      _ -> TU.failure $ "Expected no parameters for " <> q

withParameters :: forall p. String -> Query p -> _
withParameters s (Query q p) = case p of
      Just _ -> TUA.equal s q
      _ -> TU.failure $ "Expected empty parameters for " <> q






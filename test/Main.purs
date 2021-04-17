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
                        plain "SELECT 3" query
                  TU.test "field" do
                        let query = toQuery $ select id
                        plain "SELECT id" query
                  TU.test "star" do
                        let query = toQuery $ select star
                        plain "SELECT *" query

            TU.suite "columns" do
                  TU.test "scalars" do
                        let query = toQuery $ select (3 /\ 4 /\ 5)
                        plain "SELECT 3, 4, 5" query
                  TU.test "fields" do
                        let query = toQuery $ select (id /\ name)
                        plain "SELECT id, name" query
                  TU.test "mixed" do
                        let query = toQuery $ select (star /\ id /\ 5)
                        plain "SELECT *, id, 5" query

            TU.suite "sub query" do
                  TU.test "scalars" do
                        let query = toQuery $ select (3 /\ (select 34 # from users # wher (name .=. surname)))
                        plain "SELECT 3, (SELECT 34 FROM users WHERE name = surname)" query
                  TU.test "fields" do
                        let query = toQuery $ select (id /\ (select (Field :: Field "sent") # from messages))
                        plain "SELECT id, (SELECT sent FROM messages)" query
                  TU.test "mixed" do
                        let query = toQuery $ select (3 /\ (select id # from messages) /\ id /\ name /\ star /\ (select (Field :: Field "sent") # from messages) /\ (select 5 # from messages))
                        plain "SELECT 3, (SELECT id FROM messages), id, name, *, (SELECT sent FROM messages), (SELECT 5 FROM messages)" query

      TU.suite "from" do
            TU.test "table" do
                  let query = toQuery $ select 34 # from messages
                  plain "SELECT 34 FROM messages" query

      TU.suite "where" do
            TU.suite "field" do
                  TU.test "equals" do
                        let query = toQuery $ select 34 # from users # wher (name .=. surname)
                        plain "SELECT 34 FROM users WHERE name = surname" query
                  -- TU.test "not equals" do
                  --       let Query q parameters = toQuery $ select 34 # from users # wher (name .<>. surname)
                  --       TUA.equal "SELECT 34 FROM users WHERE name <> surname" q
                  --       TUA.equal (Just ) parameters

            -- TU.suite "parameters" do
            --       TU.test "equals" do
            --             let parameters = {name : "josh"}
            --             let Query q p = toQuery $ select id # from users # wher (name .=. (Parameter :: Parameter "name")) parameters
            --             TUA.equal "SELECT id FROM users WHERE name = @name" q
            --             TUA.equal (Just parameters) p
            --       TU.test "not equals" do
            --             let parameters = {id: 3}
            --             let Query q p = toQuery $ select id # from users # wher (id .<>. (Parameter :: Parameter "id")) parameters
            --             TUA.equal "SELECT id FROM users WHERE id <> @id" q
            --             TUA.equal (Just parameters) p

            -- TU.suite "logical operands" do
            --       TU.suite "and" do
            --             TU.test "single" do
            --                   let parameters = {name : "josh"}
            --                   let Query q p = toQuery $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .&&. name .=. surname) parameters
            --                   TUA.equal "SELECT id FROM users WHERE (name = @name AND name = surname)" q
            --                   TUA.equal (Just parameters) p
            --             TU.test "many" do
            --                   let parameters = {name : "josh", surname: "j."}
            --                   let Query q p = toQuery $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .&&. name .=. surname .&&. surname .<>. (Parameter :: Parameter "surname")) parameters
            --                   TUA.equal "SELECT id FROM users WHERE ((name = @name AND name = surname) AND surname <> @surname)" q
            --                   TUA.equal (Just parameters) p

            --       TU.suite "or" do
            --             TU.test "single" do
            --                   let parameters = {name : "josh"}
            --                   let Query q p = toQuery $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .||. name .=. surname) parameters
            --                   TUA.equal "SELECT id FROM users WHERE (name = @name OR name = surname)" q
            --                   TUA.equal (Just parameters) p
            --             TU.test "many" do
            --                   let parameters = {name : "josh", surname: "j."}
            --                   let Query q p = toQuery $ select id # from users # wher (name .=. (Parameter :: Parameter "name") .||. name .=. surname .||. surname .<>. (Parameter :: Parameter "surname")) parameters
            --                   TUA.equal "SELECT id FROM users WHERE ((name = @name OR name = surname) OR surname <> @surname)" q
            --                   TUA.equal (Just parameters) p

            --       TU.suite "mixed" do
            --             TU.test "not bracketed" do
            --                   let parameters = {id3 : 3, id33: 33, id333 : 333}
            --                   let Query q p = toQuery $ select id # from users # wher (id .=. (Parameter :: Parameter "id3") .||. id .=. (Parameter :: Parameter "id33") .&&. id .=. (Parameter :: Parameter "id333")) parameters
            --                   TUA.equal "SELECT id FROM users WHERE (id = @id3 OR (id = @id33 AND id = @id333))" q
            --                   TUA.equal (Just parameters) p
            --             TU.test "bracketed" do
            --                   let parameters = {id3 : 3, id33: 33, id333 : 333}
            --                   let Query q p = toQuery $ select id # from users # wher ((id .=. (Parameter :: Parameter "id3") .||. id .=. (Parameter :: Parameter "id33")) .&&. id .=. (Parameter :: Parameter "id333")) parameters
            --                   TUA.equal "SELECT id FROM users WHERE ((id = @id3 OR id = @id33) AND id = @id333)" q
            --                   TUA.equal (Just parameters) p

      -- TU.suite "as" do
      --       TU.suite "named sub queries" do
      --             TU.test "scalars" do
      --                   let query = toQuery $ select 3 # from (select 34 # from users # as u)
      --                   plain "SELECT 3 FROM (SELECT 34 FROM users) u" query
      --             TU.test "fields" do
      --                   let query = toQuery $ select id # from (select (id /\ name) # from users # as u)
      --                   plain "SELECT id FROM (SELECT id, name FROM users) u" query
      --             TU.test "table" do
      --                   let query = toQuery $ select star # from (select star # from users # as u)
      --                   plain "SELECT * FROM (SELECT * FROM users) u" query
      --             TU.test "filtered" do
      --                   let query = toQuery $ select star # from (select (id /\ name /\ surname) # from users # wher (name .=. (Parameter :: Parameter "n")) {n : "nn"} # as u)
      --                   plain "SELECT * FROM (SELECT id, name, surname FROM users WHERE name = @n) u" query

plain :: forall p. String -> Query p -> _
plain s query = case query of
      Plain q -> TUA.equal s q
      _ -> TU.failure $ "Expected no parameters for " <> s






module Test.Main where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet (Field(..), Query(..), Table(..), from, print, select, wher, (.<>.), (.=.))
import Effect (Effect)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM

type Users = (id :: Int, name :: String, surname :: String, birthday :: Date, joined :: Date)

users :: Table "users" Users
users = Table

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
                        let query = print $ select 3
                        noParameters "SELECT 3" query
                  TU.test "field" do
                        let query = print $ select id
                        noParameters "SELECT id" query
                  TU.test "table" do
                        let query = print $ select users
                        noParameters "SELECT users.*" query

            TU.suite "columns" do
                  TU.test "scalars" do
                        let query = print $ select (3 /\ 4 /\ 5)
                        noParameters "SELECT 3, 4, 5" query
                  TU.test "fields" do
                        let query = print $ select (id /\ name)
                        noParameters "SELECT id, name" query
                  TU.test "mixed" do
                        let query = print $ select (users /\ id /\ 5)
                        noParameters "SELECT users.*, id, 5" query

      TU.suite "from" do
            TU.test "table" do
                  let query = print $ select 34 # from messages
                  noParameters "SELECT 34 FROM messages" query

      TU.suite "where" do
            TU.suite "field" do
                  TU.test "equals" do
                        let query = print $ select 34 # from users # wher (name .=. surname) {}
                        emptyParameters "SELECT 34 FROM users WHERE name = surname" query
                  TU.test "not equals" do
                        let query = print $ select 34 # from users # wher (name .<>. surname) {}
                        emptyParameters "SELECT 34 FROM users WHERE name <> surname" query

            TU.suite "parameters" do
                  TU.test "equals" do
                        let query = print $ select id # from users # wher (name .=. (Field :: Field "namep")) {namep : "josh"}
                        emptyParameters "SELECT id FROM users WHERE name = @namep" query

                  TU.test "not equals" do
                        let query = print $ select id # from users # wher (id .<>. (Field :: Field "idp")) {idp: 3}
                        emptyParameters "SELECT id FROM users WHERE id <> @idp" query
            --finish
            TU.suite "and" do
                  TU.test "equals" do
                        let query = print $ select id # from users # wher (name .=. (Field :: Field "namep")) {namep : "josh"}
                        emptyParameters "SELECT id FROM users WHERE name = @namep" query

                  TU.test "not equals" do
                        let query = print $ select id # from users # wher (id .<>. (Field :: Field "idp")) {idp: 3}
                        emptyParameters "SELECT id FROM users WHERE id <> @idp" query

      TU.suite "sub query" do
            TU.suite "select" do
                  TU.test "scalars" do
                        let query = print $ select (3 /\ (select 34 # from messages))
                        noParameters "SELECT 3, (SELECT 34 FROM messages)" query
                  TU.test "fields" do
                        let query = print $ select (id /\ (select (Field :: Field "sent") # from messages))
                        noParameters "SELECT id, (SELECT sent FROM messages)" query
                  TU.test "mixed" do
                        let query = print $ select (3 /\ (select id # from messages) /\ id /\ name /\ users /\ (select (Field :: Field "sent") # from messages) /\ (select 5 # from messages))
                        noParameters "SELECT 3, (SELECT id FROM messages), id, name, users.*, (SELECT sent FROM messages), (SELECT 5 FROM messages)" query

noParameters :: forall p. String -> Query p -> _
noParameters s (Query q p) = case p of
      Nothing -> TUA.equal s q
      _ -> TU.failure $ "Expected no parameters for " <> q

emptyParameters :: forall p. String -> Query p -> _
emptyParameters s (Query q p) = case p of
      Just {} -> TUA.equal s q
      _ -> TU.failure $ "Expected empty parameters for " <> q





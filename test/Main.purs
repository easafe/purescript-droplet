module Test.Main where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet (Field(..), Query(..), Table(..), print, select)
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

type Messages = (id :: Int, name :: String, haha :: Boolean)

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
                  TU.test "tables" do
                        let query = print $ select (users /\ messages)
                        noParameters "SELECT users.*, messages.*" query

      where noParameters :: forall p. String -> Query p -> _
            noParameters s (Query q p) = case p of
                  Nothing -> TUA.equal s q
                  _ -> TU.failure $ "Expected no parameters for " <> q




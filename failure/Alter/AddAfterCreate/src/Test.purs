module Alter.AddAfterCreate.Test where

import Droplet.Language
import Prelude hiding (join, add)

import Data.Date (Date)
import Data.Tuple.Nested (type (/\), (/\))
import Type.Proxy (Proxy(..))

type Users =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , name ∷ String
      , surname ∷ String
      , birthday ∷ Column Date Default
      )

type UsersTable = Table "users" Users

users ∷ UsersTable
users = Table

id ∷ Proxy "id"
id = Proxy

name ∷ Proxy "name"
name = Proxy

surname ∷ Proxy "surname"
surname = Proxy

n ∷ Proxy "n"
n = Proxy

t ∷ Proxy "t"
t = Proxy

u ∷ Proxy "u"
u = Proxy

test = create # table users # add (Proxy ∷ _ "nickname") (Proxy ∷ _ String)

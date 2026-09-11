module From.ColumnDoesNotExist.Test where

import Droplet.Language
import Prelude hiding (join)

import Data.Date (Date)
import Data.Tuple.Nested (type (/\), (/\))
import Type.Proxy (Proxy(..))

type Users =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , name ∷ String
      , surname ∷ String
      , birthday ∷ Column Date Default
      , joined ∷ Column Date Default
      )

type UsersTable = Table "users" Users

users ∷ UsersTable
users = Table

test = select anniversary # from users
      where
      anniversary = Proxy ∷ Proxy "anniversary"
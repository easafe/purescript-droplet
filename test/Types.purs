module Test.Types where

import Prelude

import Prim hiding (Constraint)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Droplet.Language
import Type.Proxy (Proxy(..))

type UsersColumns =
      ( id ∷ Int
      , name ∷ String
      , surname ∷ String
      , birthday ∷ Date
      , joined ∷ Date
      )

type UserConstraints =
      ( Constraint "pk_users" "id" PrimaryKey
              /\ Constraint "id_users" "id" Identity
              /\ Constraint "df_birthday" "birthday" Default
              /\ Constraint "df_joined" "joined" Default
      )

type MessagesColumns =
      ( id ∷ Int
      , sender ∷ Int
      , recipient ∷ Int
      , date ∷ DateTime
      , second_date ∷ DateTime
      , sent ∷ Boolean
      )

type MessagesConstraints =
      ( Constraint "pk_messages" "id" PrimaryKey
              /\ Constraint "id_messages" "id" Identity
              /\ Constraint "fk_sender" "sender" (ForeignKey "id" TableUsers)
              /\ Constraint "fk_recipient" "recipient" (ForeignKey "id" TableUsers)
              /\ Constraint "df_date" "date" Default
              /\ Constraint "df_second_date" "second_date" Default
      )

type TagsColumns =
      ( id ∷ Int
      , name ∷ String
      , created ∷ Maybe Date
      , by ∷ Maybe Int
      )

type TagsConstraints =
      ( Constraint "pk_tags" "id" PrimaryKey
              /\ Constraint "id_tags" "id" Identity
      )

type MaybeKeysColumns =
      ( id ∷ Int
      )

type MaybeKeysConstraints =
      ( Constraint "pk_maybe_keys" "id" PrimaryKey
      )

type UniqueValuesColumns =
      ( name ∷ String
      , by ∷ Maybe Int
      )

type UniqueValuesConstraints =
      ( Constraint "uq_name" "name" Unique
              /\ Constraint "uq_by" "by" Unique
      )

type DefaultColumnsColumns =
      ( recipient ∷ ColumnRecipient
      , sender ∷ ColumnSender
      )

type DefaultColumnsConstraints =
      ( Constraint "uq_recipient" "recipient" Unique
            /\ Constraint "df_recipient" "recipient" Default
              /\ Constraint "df_sender" "sender" Default
      )

--needs table with composite pk fk unique

newtype ColumnSender = ColumnSender Int
newtype ColumnRecipient = ColumnRecipient Int

instance ToValue ColumnSender where
      toValue (ColumnSender x) = toValue x

instance ToValue ColumnRecipient where
      toValue (ColumnRecipient x) = toValue x

instance FromValue ColumnSender where
      fromValue x = ColumnSender <$> fromValue x

instance FromValue ColumnRecipient where
      fromValue x = ColumnRecipient <$> fromValue x

derive instance Eq ColumnSender
derive instance Eq ColumnRecipient

instance Show ColumnSender where
      show (ColumnSender x) = show x

instance Show ColumnRecipient where
      show (ColumnRecipient x) = show x

type TableUsers = Table "users" UsersColumns UserConstraints

users ∷ TableUsers
users = Table

messages ∷ Table "messages" MessagesColumns MessagesConstraints
messages = Table

tags ∷ Table "tags" TagsColumns TagsConstraints
tags = Table

maybeKeys ∷ Table "maybe_keys" MaybeKeysColumns MaybeKeysConstraints
maybeKeys = Table

uniqueValues ∷ Table "unique_values" UniqueValuesColumns UniqueValuesConstraints
uniqueValues = Table

defaultColumns ∷ Table "default_columns" DefaultColumnsColumns DefaultColumnsConstraints
defaultColumns = Table

id ∷ Proxy "id"
id = Proxy

name ∷ Proxy "name"
name = Proxy

surname ∷ Proxy "surname"
surname = Proxy

sent ∷ Proxy "sent"
sent = Proxy

date ∷ Proxy "date"
date = Proxy

joined ∷ Proxy "joined"
joined = Proxy

birthday ∷ Proxy "birthday"
birthday = Proxy

sender ∷ Proxy "sender"
sender = Proxy

recipient ∷ Proxy "recipient"
recipient = Proxy

second_date ∷ Proxy "second_date"
second_date = Proxy

created ∷ Proxy "created"
created = Proxy

_by ∷ Proxy "by"
_by = Proxy

b ∷ Proxy "b"
b = Proxy

n ∷ Proxy "n"
n = Proxy

t ∷ Proxy "t"
t = Proxy

u ∷ Proxy "u"
u = Proxy

bigB ∷ Proxy "B"
bigB = Proxy

date_part_age ∷ FunctionSignature (String /\ DateTime) Int
date_part_age = function "date_part_age"

date_part_age' ∷ FunctionSignature (String /\ Date) Int
date_part_age' = function "date_part_age"

fire_missiles ∷ FunctionSignature (Int /\ Int) Unit
fire_missiles = function "fire_missiles"

utc_now ∷ FunctionSignature' DateTime
utc_now = function' "utc_now"
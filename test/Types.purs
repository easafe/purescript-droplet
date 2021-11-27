module Test.Types where

import Prelude

import Prim hiding (Constraint)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Droplet.Language
import Type.Proxy (Proxy(..))

type Users =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , name ∷ String
      , surname ∷ String
      , birthday ∷ Column Date Default
      , joined ∷ Column Date Default
      )

type UsersTable = Table "users" Users

type Messages =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , sender ∷ Column Int (NamedConstraint "sender_user" (ForeignKey "id" UsersTable))
      , recipient ∷ Int
      , date ∷ Column Date Default
      , secondDate ∷ ColumnDefinition Date "second_date" Default
      , sent ∷ Boolean
      )

type Tags =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , name ∷ String
      , created ∷ Maybe Date
      , by ∷ Column (Maybe Int) (NamedConstraint "by_user" (ForeignKey "id" UsersTable))
      )

type MaybeKeys =
      ( id ∷ Column Int PrimaryKey
      )

type UniqueValues =
      ( name ∷ Column String Unique
      , by ∷ Column (Maybe Int) Unique
      )

type DefaultColumns =
      ( sender ∷ Column SenderColumn (Constraint (Unique /\ Default))
      , recipient ∷ Column RecipientColumn Default
      )

type DoublePrimaryKey =
      ( id ∷ Column Int (ConstraintDefinition "pk_double_primary_key" (Proxy "id" /\ Proxy "secondId") (PrimaryKey /\ Identity))
      , secondId ∷ RenamedColumn Int "second_id"
      )

type DoublePrimaryKeyTable = Table "double_primary_key" DoublePrimaryKey

type Composite =
      ( id ∷ Column Int (Identity /\ ConstraintDefinition "pk_composite" (Proxy "id" /\ Proxy "secondId") PrimaryKey)
      , secondId ∷ RenamedColumn Int "second_id"
      , create ∷ Maybe Date
      , name ∷ String
      , sender ∷ Column Int (ConstraintDefinition "sr_user" (Proxy "sender" /\ Proxy "recipient") (ForeignKey (Proxy "id" /\ Proxy "secondId") DoublePrimaryKeyTable))
      , recipient ∷ Int
      )

newtype SenderColumn = SenderColumn Int
newtype RecipientColumn = RecipientColumn Int

instance ToValue SenderColumn where
      toValue (SenderColumn x) = toValue x

instance ToValue RecipientColumn where
      toValue (RecipientColumn x) = toValue x

instance FromValue SenderColumn where
      fromValue x = SenderColumn <$> fromValue x

instance FromValue RecipientColumn where
      fromValue x = RecipientColumn <$> fromValue x

derive instance Eq SenderColumn
derive instance Eq RecipientColumn

instance Show SenderColumn where
      show (SenderColumn x) = show x

instance Show RecipientColumn where
      show (RecipientColumn x) = show x

users ∷ UsersTable
users = Table

messages ∷ Table "messages" Messages
messages = Table

tags ∷ Table "tags" Tags
tags = Table

maybeKeys ∷ Table "maybe_keys" MaybeKeys
maybeKeys = Table

uniqueValues ∷ Table "unique_values" UniqueValues
uniqueValues = Table

defaultColumns ∷ Table "default_columns" DefaultColumns
defaultColumns = Table

doublePrimaryKey ∷ DoublePrimaryKeyTable
doublePrimaryKey = Table

composite ∷ Table "composite" Composite
composite = Table

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

secondDate ∷ Proxy "second_date"
secondDate = Proxy

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
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
      , sender ∷ Column Int (Constraint "sender_user" (ForeignKey "id" UsersTable))
      , recipient ∷ Int
      , date ∷ Column DateTime Default
      , second_date ∷ Column DateTime Default
      , sent ∷ Boolean
      )

type Tags =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , name ∷ String
      , created ∷ Maybe Date
      , by ∷ Column (Maybe Int) (Constraint "by_user" (ForeignKey "id" UsersTable))
      )

type MaybeKeys =
      ( id ∷ Column Int PrimaryKey
      )

type UniqueValues =
      ( name ∷ Column String Unique
      , by ∷ Column (Maybe Int) Unique
      )

type DefaultColumns =
      ( sender ∷ Column SenderColumn (Unique /\ Default)
      , recipient ∷ Column RecipientColumn Default
      )

type DoublePrimaryKey =
      ( id ∷ Column Int (Identity /\ Constraint (Composite "pk_double_primary_key") PrimaryKey)
      , second_id ∷ Column Int (Identity /\ Constraint (Composite "pk_double_primary_key") PrimaryKey)
      )

type DoublePrimaryKeyTable = Table "double_primary_key" DoublePrimaryKey

type Migrated =
      ( id ∷ Column Int PrimaryKey)


type CompositeT =
      ( id ∷ Column Int (Identity /\ Constraint (Composite "pk_composite") PrimaryKey)
      , second_id ∷ Column Int (Constraint (Composite "pk_composite") PrimaryKey)
      , create ∷ Maybe Date
      , name ∷ String
      , sender ∷ Column Int (Constraint (Composite "sr_user") (ForeignKey "id" DoublePrimaryKeyTable))
      , recipient ∷ Column Int (Constraint (Composite "sr_user") (ForeignKey "second_id" DoublePrimaryKeyTable))
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

composite ∷ Table "composite" CompositeT
composite = Table

migrated ∷ Table "migrated" Migrated
migrated = Table

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

secondId ∷ Proxy "second_id"
secondId = Proxy

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
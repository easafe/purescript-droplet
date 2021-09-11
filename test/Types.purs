module Test.Types where

import Prelude

import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Droplet.Language
import Type.Proxy (Proxy(..))

type Users =
      ( id ∷ Auto Int
      , name ∷ String
      , surname ∷ String
      , birthday ∷ Default Date
      , joined ∷ Default Date
      )

type Messages =
      ( id ∷ Auto Int
      , sender ∷ Int
      , recipient ∷ Int
      , date ∷ Default DateTime
      , second_date ∷ Default DateTime
      , sent ∷ Boolean
      )

type Tags =
      ( id ∷ Auto Int
      , name ∷ String
      , created ∷ Maybe Date
      , by ∷ Maybe Int
      )

users ∷ Table "users" Users
users = Table

messages ∷ Table "messages" Messages
messages = Table

tags ∷ Table "tags" Tags
tags = Table

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

bigB :: Proxy "B"
bigB = Proxy

date_part_age ∷ FunctionSignature (String /\ DateTime) Int
date_part_age = function "date_part_age"

date_part_age' ∷ FunctionSignature (String /\ Date) Int
date_part_age' = function "date_part_age"

fire_missiles ∷ FunctionSignature (Int /\ Int) Unit
fire_missiles = function "fire_missiles"

utc_now ∷ FunctionSignature' DateTime
utc_now = function' "utc_now"
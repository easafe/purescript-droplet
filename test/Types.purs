module Test.Types where

import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Droplet (Table(..))
import Droplet.Internal.Edsl.Definition (Default, Auto)
import Type.Proxy (Proxy(..))

type Users = (
    id :: Auto Int,
    name :: String,
    surname :: String,
    birthday :: Default Date,
    joined :: Default Date
)
type Messages = (
    id :: Auto Int,
    sender :: Int,
    recipient :: Int,
    date :: Default DateTime,
    second_date :: Default DateTime,
    sent :: Boolean
)
type Tags = (
    id :: Auto Int,
    name :: String,
    created :: Maybe Date,
    by :: Maybe Int
)

users :: Table "users" Users
users = Table

messages :: Table "messages" Messages
messages = Table

tags :: Table "tags" Tags
tags = Table

id :: Proxy "id"
id = Proxy

name :: Proxy "name"
name = Proxy

surname :: Proxy "surname"
surname = Proxy

sent :: Proxy "sent"
sent = Proxy

date :: Proxy "date"
date = Proxy

joined :: Proxy "joined"
joined = Proxy

birthday :: Proxy "birthday"
birthday = Proxy

sender :: Proxy "sender"
sender = Proxy

recipient :: Proxy "recipient"
recipient = Proxy

secondDate :: Proxy "second_date"
secondDate = Proxy

b :: Proxy "b"
b = Proxy

n :: Proxy "n"
n = Proxy

t :: Proxy "t"
t = Proxy

u :: Proxy "u"
u = Proxy
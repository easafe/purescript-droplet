module Test.Types where

import Data.Date (Date)
import Data.DateTime (DateTime)
import Droplet (Alias(..), Field(..), Table(..))

type Users = (id :: Int, name :: String, surname :: String, birthday :: Date, joined :: Date)
type Messages = (id :: Int, sender :: Int, recipient :: Int, date :: DateTime, sent :: Boolean)

users :: Table "users" Users
users = Table

messages :: Table "messages" Messages
messages = Table

id :: Field "id"
id = Field

name :: Field "name"
name = Field

surname :: Field "surname"
surname = Field

sent :: Field "sent"
sent = Field

date :: Field "date"
date = Field

joined :: Field "joined"
joined = Field

birthday :: Field "birthday"
birthday = Field

b :: Alias "b"
b = Alias

n :: Alias "n"
n = Alias

t :: Alias "t"
t = Alias

u :: Alias "u"
u = Alias
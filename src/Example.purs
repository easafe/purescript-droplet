module Example where

import Data.Date
import Droplet
import Prelude

import Data.Tuple.Nested ((/\))

type Users = (id :: Int, name :: String, surname :: String, birthday :: Date, joined :: Date)

users :: Table "users" Users
users = Table

type Messages = (id :: Int, name :: String, haha :: Boolean)

messages :: Table "messages" Messages
messages = Table

id :: Field "id"
id = Field

name :: Field "name"
name = Field

haha :: Field "haha"
haha = Field

joined :: Field "joined"
joined = Field

--select

selonly1 = select 1
selonly2 = select users
selonly3 = select ((Field :: Field "id") /\ (Field :: Field "joined"))
selonly4 = select ((Field :: Field "id") /\ (Field :: Field "name"))
selonly5 = select (Field :: Field "name")
selonly6 = select id
selonly7 = select (id /\ name)

subselect1 = select (select (Field :: Field "id") # from messages)
subselect2 = select (select 23 # from messages)
subselect3 = select (select id # from messages)
subselect4 = select (users /\ (select id # from messages))
subselect5 = select (id /\ (select 55 # from messages))
subselect6 = select (name /\ id /\ (Field :: Field "joined") /\ (select haha # from messages))
subselect7 = select (id /\ (select id # from messages # wher ((Field :: Field "name") .=. (Field :: Field "nameP")) {nameP : "jesus"}))

--from

fromonly1 = from users
fromonly2 = from messages

-- where

whereonly1 = wher (name .=. (Field :: Field "surname") .&&. (Field :: Field "birthday") .=. (Field :: Field "joined")) {}
whereonly2 = wher (name .=. (Field :: Field "parameter1") .&&. (Field :: Field "birthday") .=. joined) { parameter1 : "oio"}

--select from

selfrom1 = selonly2 # fromonly1
selfrom2 = selonly3 # fromonly1
selfrom3 = selonly4 # fromonly2
selfrom4 = selonly1 # fromonly2
selfrom5 = select id # fromonly2
selfrom6 = selonly7 # fromonly2

--select from where

selfromwhere1 = selonly2 # fromonly1 # whereonly1
selfromwhere2 = selonly3 # fromonly1 # whereonly2
selfromwhere3 = selonly1 # fromonly1 # whereonly2

--select sub select from

selsubselect1 = subselect1 # fromonly1
selsubselect2 = subselect2 # fromonly1
selsubselect3 = subselect3 # fromonly2
selsubselect4 = subselect4 # fromonly1
selsubselect5 = subselect5 # fromonly2
selsubselect6 = subselect6 # fromonly1

s = print $ select (3 /\ (select 34 # from messages))




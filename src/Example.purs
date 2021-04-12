module Example where

import Data.Date
import Droplet
import Prelude

import Data.Tuple (Tuple(..))
import Prim.Row (class Union)
import Type.Proxy (Proxy(..))

type Users = (id :: Int, name :: String, surname :: String, birthday :: Date, joined :: Date)

users :: Table "users" Users
users = table

type Messages = (id :: Int, name :: String)

messages :: Table "messages" Messages
messages = table

id :: Field "id"
id = Field

--only select

selonly1 = select 1
selonly2 = select (Proxy :: Proxy Users)
selonly3 = select (Proxy :: Proxy (id :: Int, joined :: Date))
selonly4 = select (Proxy :: Proxy (id :: Int, name :: String))
selonly5 = select (Field :: Field "name")
selonly6 = select id

subselect1 = select (select (Proxy :: Proxy (id :: Int)) # from messages)
subselect2 = select (select 23 # from messages)
--subselect3 = select (Tuple (Proxy :: Proxy Users) (select (Proxy :: Proxy (id :: Int, name :: String)) # from messages))

--only from

fromonly1 = from users
fromonly2 = from messages

-- only where

whereonly1 = wher ((Proxy :: Proxy "name") .=. (Proxy :: Proxy "surname") .&&. (Proxy :: Proxy "birthday") .=. (Proxy :: Proxy "joined")) {}
whereonly2 = wher ((Proxy :: Proxy "name") .=. (Proxy :: Proxy "parameter1") .&&. (Proxy :: Proxy "birthday") .=. (Proxy :: Proxy "joined")) { parameter1 : "oio"}

--select from

selfrom1 = selonly2 # fromonly1
selfrom2 = selonly3 # fromonly1
selfrom3 = selonly4 # fromonly2
selfrom4 = selonly1 # fromonly2

--select from wher

selfromwhere1 = selonly2 # fromonly1 # whereonly1
selfromwhere2 = selonly3 # fromonly1 # whereonly2
selfromwhere3 = selonly1 # fromonly1 # whereonly2

--subselect

selsubselect1 = subselect1 # fromonly1
selsubselect2 = subselect2 # fromonly1


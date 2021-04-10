module Example where

import Data.Date
import Droplet
import Prelude

import Prim.Row (class Union)
import Type.Proxy (Proxy(..))

type Users = (id :: Int, name :: String, surname :: String, birthday :: Date, joined :: Date)

users :: Table "users" Users
users = table

type Messages = (id :: Int, name :: String)

messages :: Table "messages" Messages
messages = table

--only select

selonly1 = select 1
selonly2 = select (Proxy :: Proxy Users)
selonly3 = select (Proxy :: Proxy (id :: Int, joined :: Date))
selonly4 = select (Proxy :: Proxy (id :: Int, name :: String))

subselect1 = select (Proxy :: Proxy (id :: Int, name :: String))

--only from

fromonly1 = from users
fromonly2 = from messages

-- only where

--use the operators where
whereonly1 = wher ((Proxy :: Proxy "name") .=. (Proxy :: Proxy "surname") .&&. (Proxy :: Proxy "birthday") .=. (Proxy :: Proxy "joined")) {}
whereonly2 = wher ((Proxy :: Proxy "name") .=. (Proxy :: Proxy "parameter1") .&&. (Proxy :: Proxy "birthday") .=. (Proxy :: Proxy "joined")) { parameter1 : "oio"}

--select from

selfrom1 = selonly2 # fromonly1
selfrom2 = selonly3 # fromonly1
selfrom3 = selonly4 # fromonly2
selfrom4 = selonly1 # fromonly2

--select from where
selfromwhere1 = selonly2 # fromonly1 # whereonly1
selfromwhere2 = selonly3 # fromonly1 # whereonly2
selfromwhere3 = selonly1 # fromonly1 # whereonly2


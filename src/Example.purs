module Example where

import Data.Date
import Droplet
import Prelude

import Type.Proxy (Proxy(..))

type Users = (id :: Int, name :: String, surname :: String, birthday :: Date, joined :: Date)

users :: Table Users "users"
users = table

type Messages = (id :: Int, name :: String)

messages :: Table Messages "messages"
messages = table

--select all fields

sel1 = select (Proxy :: Proxy Users) $ from users

--shouldnt type check
--selerr1 = select (Proxy :: Proxy Users) $ from messages
--selerr2 = select (Proxy :: Proxy (id :: Int, when :: Date)) $ from users
--selerr3 = select (Proxy :: Proxy (id :: Int)) (from users) (wher (((Proxy :: Proxy "name") `equals` (Proxy :: Proxy "surname")) `and` ((Proxy :: Proxy "birthday") `equals` (Proxy :: Proxy "id"))))



--select some fields
sel2 = select (Proxy :: Proxy (id :: Int)) $ from users
sel3 = select (Proxy :: Proxy (id :: Int, birthday :: Date)) $ from users

--where
sel4 = select (Proxy :: Proxy (id :: Int)) (from users) $ wher $ ((Proxy :: Proxy "name") `equals` (Proxy :: Proxy "surname")) `and` ((Proxy :: Proxy "birthday") `equals` (Proxy :: Proxy "joined"))

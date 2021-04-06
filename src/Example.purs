module Example where

import Droplet
import Prelude
import Data.Date

import Type.Proxy (Proxy(..))

type Users = (id :: Int, name :: String, birthday :: Date)

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

--select some fields
sel2 = select (Proxy :: Proxy (id :: Int)) $ from users
sel3 = select (Proxy :: Proxy (id :: Int, birthday :: Date)) $ from users

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
--sel2 = select (Proxy :: Proxy Users) $ from messages

--select some fields
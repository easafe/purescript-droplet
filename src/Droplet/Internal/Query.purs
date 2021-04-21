-- | This module define `ToQuery`, a type class to generate (intermediate form) parameterized SQL statements strings
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet` instead
module Droplet.Internal.Query where

import Droplet.Internal.Definition
import Droplet.Internal.Filter
import Droplet.Internal.Language
import Prelude

import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe as EEU
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce as UC

data Prepared
data Other

--cant be a functor because of parameters kind
data Query parameters =
      Parameterized String (Record parameters) |
      Plain String

--for debugging
instance queryShow :: Show (Query parameters) where
      show = case _ of
            Parameterized q _ -> q
            Plain q -> q

extractPlain :: forall parameters. Query parameters -> String
extractPlain = case _ of
      Plain query -> query
      _ -> EEU.unsafeThrow "Tried to append to parameterized query"

--magic strings
selectKeyword :: String
selectKeyword = "SELECT "

fromKeyword :: String
fromKeyword = " FROM "

whereKeyword :: String
whereKeyword = " WHERE "

andKeyword :: String
andKeyword = " AND "

orKeyword :: String
orKeyword = " OR "

asKeyword = " AS "

starToken :: String
starToken = "*"

comma :: String
comma = ", "

openBracket :: String
openBracket = "("

closeBracket :: String
closeBracket = ")"

equalsSymbol :: String
equalsSymbol = " = "

notEqualsSymbol = " <> "

-- use this instead of toQuery
query :: forall q parameters. ToQuery q Other => q -> Query parameters
query q = toQuery q (Proxy :: Proxy Other)

--any way to not have to pass the proxy around?
class ToQuery q (starting :: Type) where
      toQuery :: forall parameters. q -> Proxy starting -> Query parameters



----------------------PREPARE----------------------------

instance prepareToQuery :: ToQuery q Prepared => ToQuery (Prepare q parameters) starting where
      toQuery (Prepare q parameters) _ = Parameterized (extractPlain $ toQuery q (Proxy :: Proxy Prepared)) $ UC.unsafeCoerce parameters



----------------------SELECT----------------------------


instance subSelectWhereToQuery :: ToQuery (Where w s has to) starting => ToQuery (Select (Where w s has to) parameters fields) starting where
      toQuery (Select wr) s = Plain $ openBracket <> extractPlain (toQuery wr s) <> closeBracket
else
instance subSelectFromToQuery :: ToQuery (From f s to) starting => ToQuery (Select (From f s to) parameters fields) starting where
      toQuery (Select fr) s = Plain $ openBracket <> extractPlain (toQuery fr s) <> closeBracket
else
instance selectToQuery :: ToQuery s starting => ToQuery (Select s parameters fields) starting where
      toQuery (Select s) st = Plain $ selectKeyword <> extractPlain (toQuery s st)

instance selectFieldToQuery :: IsSymbol name => ToQuery (Field name) starting where
      toQuery _ _ = Plain $ DS.reflectSymbol (Proxy :: Proxy name)

instance tableToQuery :: ToQuery Star starting where
      toQuery _ _ = Plain starToken

instance intScalarToQuery :: ToQuery Int starting where
      toQuery n _ = Plain $ show n

instance selectTupleToQuery :: (ToQuery s starting, ToQuery t starting) => ToQuery (Tuple (Select s parameters to) (Select t parameters fields)) starting where
      toQuery (Tuple (Select s) (Select t)) st = Plain $ extractPlain (toQuery s st)  <> comma <> extractPlain (toQuery t st)



-------------------------------FROM----------------------------

instance subSelectfromTableToQuery :: (IsSymbol name, ToQuery s starting) => ToQuery (From (Table name fields) s fields) starting where
      toQuery (From _ s) st = Plain $ extractPlain (toQuery s st) <> fromKeyword <> DS.reflectSymbol (Proxy :: Proxy name)



-------------------------------WHERE----------------------------

instance whereFailToQuery :: Fail (Text "Since this query references parameters, Droplet.prepare must be used") => ToQuery (Where f fields Parameterized parameters) Other where
      toQuery _ _ = Plain "impossible"
else
instance whereToQuery :: ToQuery f starting => ToQuery (Where f fields has parameters) starting where
      toQuery (Where filtered fr) st = Plain $ extractPlain (toQuery fr st) <> whereKeyword <> printFilter filtered
            where printFilter = case _ of
                        Operation field otherField op -> field <> printOperator op <> otherField
                        And filter otherFilter -> openBracket <> printFilter filter <> andKeyword <> printFilter otherFilter <> closeBracket
                        Or filter otherFilter -> openBracket <> printFilter filter <> orKeyword <> printFilter otherFilter <> closeBracket

                  printOperator = case _ of
                        Equals -> equalsSymbol
                        NotEquals -> notEqualsSymbol


----------------------------AS----------------------------

instance fromAsToQuery :: (ToQuery q starting, ToQuery s starting, IsSymbol name) => ToQuery (From (As q name projection) s projection) starting where
      toQuery (From (As q) s) st = Plain $
            extractPlain (toQuery s st) <>
            fromKeyword <>
            openBracket <>
            extractPlain (toQuery q st) <>
            closeBracket <>
            asKeyword <>
            DS.reflectSymbol (Proxy :: Proxy name)
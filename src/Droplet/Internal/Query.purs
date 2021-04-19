-- | This module define `ToQuery`, a type class to generate parameterized SQL statements strings
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
import Debug (spy)
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

starToken :: String
starToken = "*"

comma :: String
comma = ", "

openBracket :: String
openBracket = "("

closeBracket :: String
closeBracket = ")"

--staring trick is not working
class ToQuery q (starting :: Type) | q -> starting where
      toQuery :: forall parameters. q -> Query parameters

----------------------PREPARE----------------------------

instance prepareToQuery :: ToQuery q Prepared => ToQuery (Prepare q parameters) Prepared where
      toQuery (Prepare q parameters) = Parameterized (extractPlain $ toQuery q) $ UC.unsafeCoerce parameters



----------------------SELECT----------------------------


instance subSelectWhereToQuery :: ToQuery (Where w s has to) starting => ToQuery (Select (Where w s has to) fields) starting where
      toQuery (Select wr) = Plain $ openBracket <> extractPlain (toQuery wr) <> closeBracket
else
instance subSelectFromToQuery :: ToQuery (From f s to) starting => ToQuery (Select (From f s to) fields) starting where
      toQuery (Select fr) = Plain $ openBracket <> extractPlain (toQuery fr) <> closeBracket
else
instance selectToQuery :: ToQuery s starting => ToQuery (Select s fields) starting where
      toQuery (Select s) = Plain $ selectKeyword <> extractPlain (toQuery s)

instance selectFieldToQuery :: IsSymbol name => ToQuery (Field name) starting where
      toQuery _ = Plain $ DS.reflectSymbol (Proxy :: Proxy name)

instance tableToQuery :: ToQuery Star starting where
      toQuery _ = Plain starToken

instance intScalarToQuery :: ToQuery Int starting where
      toQuery n = Plain $ show n

instance selectTupleToQuery :: (ToQuery s starting, ToQuery t starting) => ToQuery (Tuple (Select s to) (Select t to)) starting where
      toQuery (Tuple (Select s) (Select t)) = Plain $ extractPlain (toQuery s) <> comma <> extractPlain (toQuery t)



-------------------------------FROM----------------------------

instance subSelectfromTableToQuery :: (IsSymbol name, ToQuery s starting) => ToQuery (From (Table name fields) s fields) starting where
      toQuery (From _ s) = Plain $ extractPlain (toQuery s) <> fromKeyword <> DS.reflectSymbol (Proxy :: Proxy name)



-------------------------------WHERE----------------------------

instance wherToQuery :: ToQuery f starting => ToQuery (Where f fields Parameterized parameters) Prepared where
      toQuery (Where filtered fr) = Plain $ extractPlain (toQuery fr) <> whereKeyword <> printFilter filtered
            where printFilter = case _ of
                        Operation field otherField op -> field <> printOperator op <> otherField
                        And filter otherFilter -> openBracket <> printFilter filter <> andKeyword <> printFilter otherFilter <> closeBracket
                        Or filter otherFilter -> openBracket <> printFilter filter <> orKeyword <> printFilter otherFilter <> closeBracket

                  printOperator = case _ of
                        Equals -> " = "
                        NotEquals -> " <> "
else
instance wher2ToQuery :: ToQuery f starting => ToQuery (Where f fields NotParameterized parameters) starting where
      toQuery (Where filtered fr) = Plain $ extractPlain (toQuery fr) <> whereKeyword <> printFilter filtered
            where printFilter = case _ of
                        Operation field otherField op -> field <> printOperator op <> otherField
                        And filter otherFilter -> openBracket <> printFilter filter <> andKeyword <> printFilter otherFilter <> closeBracket
                        Or filter otherFilter -> openBracket <> printFilter filter <> orKeyword <> printFilter otherFilter <> closeBracket

                  printOperator = case _ of
                        Equals -> " = "
                        NotEquals -> " <> "

else
instance wher3FailToQuery :: Fail (Text "Parameters must be provided via Droplet.prepared") => ToQuery (Where f fields Parameterized parameters) Other where
      toQuery _ = Plain "impossible"


----------------------------AS----------------------------

-- instance fromAsToQuery :: (ToQuery f, ToQuery s, ToQuery s2, IsSymbol name) => ToQuery (FromAs (As (From f (Select s fields) fields) (Alias name) projection) (Select s2 projection) projection) starting where
--       toQuery (FromAs (As asf) s) = Query q Nothing
--             where Query sel _ = toQuery s
--                   Query aliased _ = toQuery asf
--                   q = sel <> " FROM (" <> aliased <> ") " <> DS.reflectSymbol (Proxy :: Proxy name)

-- instance fromAsWhereToQuery :: (ToWhereQuery f, ToQuery s, ToQuery s2, IsSymbol name) => ToQuery (FromAs (As (Where f fields parameters) (Alias name) projection) (Select s projection) projection) starting where
--       toQuery (FromAs (As asf) s) = Query q parameters
--             where Query sel _ = toQuery s
--                   Query aliased parameters = toQuery asf
--                   q = sel <> " FROM (" <> aliased <> ") " <> DS.reflectSymbol (Proxy :: Proxy name)




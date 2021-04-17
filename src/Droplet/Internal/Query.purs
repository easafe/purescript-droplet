-- | This module define `ToQuery`, a type class to generate parameterized SQL statements strings
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet` instead
module Droplet.Internal.Query where

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

--cant be a functor because of parameters kind
data Query parameters =
      Parameterized String (Record parameters) |
      Plain String

data Prepared

data Other

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

--literally only Prepare has parameters, can this be simplified?
class ToQuery q (starting :: Type) | q -> starting where
      toQuery :: forall parameters. q -> Query parameters

instance prepareToQuery :: ToQuery q Prepared => ToQuery (Prepare q parameters) Prepared where
      toQuery (Prepare q parameters) = Parameterized (extractPlain $ toQuery q) $ UC.unsafeCoerce parameters

instance selectToQuery :: ToQuery s starting => ToQuery (Select s fields) starting where
      toQuery (Select s) = Plain $ selectKeyword <> extractPlain (toQuery s)

instance selectFieldToQuery :: IsSymbol name => ToQuery (SelectField name) starting where
      toQuery _ = Plain $ DS.reflectSymbol (Proxy :: Proxy name)

instance tableToQuery :: ToQuery SelectStar starting where
      toQuery _ = Plain starToken

instance intScalarToQuery :: ToQuery (SelectScalar Int) starting where
      toQuery (SelectScalar n) = Plain $ show n

instance selectTupleToQuery :: (ToQuery s starting, ToQuery t starting) => ToQuery (SelectTuple (Tuple (Select s f) (Select t g))) starting where
      toQuery (SelectTuple (Tuple (Select s) (Select t))) = Plain $ extractPlain (toQuery s) <> comma <> extractPlain (toQuery t)

instance subSelectFromToQuery :: ToQuery f starting => ToQuery (SubSelectFrom f s fields) starting where
      toQuery (SubSelectFrom fr) = Plain $ openBracket <> extractPlain (toQuery fr) <> closeBracket

instance subSelectWherFailToQuery :: Fail (Text "Parameters must be provided via Droplet.prepared") => ToQuery (SubSelectWhere w s Parameterized fields) Other where
      toQuery _ = Plain ""
else
instance subSelectWhereToQuery :: ToQuery w starting=> ToQuery (SubSelectWhere w s has fields) starting where
      toQuery (SubSelectWhere wr) = Plain $ openBracket <> extractPlain (toQuery wr) <> closeBracket

instance fromToQuery :: ToQuery f starting => ToQuery (From f s fields) starting where
      toQuery (From fr) = toQuery fr

instance fromTableToQuery :: (IsSymbol name, ToQuery s starting) => ToQuery (FromTable name s fields) starting where
      toQuery (FromTable s) = Plain $ extractPlain (toQuery s) <> fromKeyword <> DS.reflectSymbol (Proxy :: Proxy name)

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

instance wherFailToQuery :: Fail (Text "Parameters must be provided via Droplet.prepared") => ToQuery (Where f fields Parameterized parameters) Other where
      toQuery _ = Plain ""
else
instance wherToQuery :: ToQuery f starting => ToQuery (Where f fields has parameters) starting where
      toQuery (Where filtered fr) = Plain $ extractPlain (toQuery fr) <> whereKeyword <> printFilter filtered
            where printFilter = case _ of
                        Operation field otherField op -> field <> printOperator op <> otherField
                        And filter otherFilter -> openBracket <> printFilter filter <> andKeyword <> printFilter otherFilter <> closeBracket
                        Or filter otherFilter -> openBracket <> printFilter filter <> orKeyword <> printFilter otherFilter <> closeBracket

                  printOperator = case _ of
                        Equals -> " = "
                        NotEquals -> " <> "
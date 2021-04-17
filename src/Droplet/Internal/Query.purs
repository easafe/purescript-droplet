-- | This module define `ToQuery`, a type class to generate parameterized SQL statements strings
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet` instead
module Droplet.Internal.Query where

import Droplet.Internal.Language
import Prelude

import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Prim.Row (class Nub, class Union)
import Record as R
import Type.Proxy (Proxy(..))

data Query parameters = Query String Suffix (Record parameters)

newtype Suffix = Suffix Int

--for debugging
instance queryShow :: Show (Query parameters) where
      show (Query q _ _) = q

--for testing
peek :: forall parameters q. ToQuery q parameters => q -> Tuple String (Record parameters)
peek q =  s /\ p
      where Query s _ p = toQuery q $ Suffix 0

--magic strings
selectKeyword :: String
selectKeyword = "SELECT "

fromKeyword = " FROM "

whereKeyword = " WHERE "

star :: String
star = "*"

comma :: String
comma = ", "

openBracket :: String
openBracket = "("

closeBracket :: String
closeBracket = ")"

class ToQuery q parameters | q -> parameters where
      toQuery :: q -> Suffix -> Query parameters

instance selectToQuery :: ToSelectQuery s parameters => ToQuery (Select s fields) parameters where
      toQuery (Select s) suffix = Query (selectKeyword <> query) newSuffix parameters
            where Query query newSuffix parameters = toSelectQuery s suffix

class ToSelectQuery s parameters | s -> parameters where
      toSelectQuery :: s -> Suffix -> Query parameters

instance selectFieldToSelectQuery :: IsSymbol name => ToSelectQuery (SelectField name) () where
      toSelectQuery _ suffix = Query (DS.reflectSymbol (Proxy :: Proxy name)) suffix {}

instance tableToSelectQuery :: ToSelectQuery SelectStar () where
      toSelectQuery _ suffix = Query star suffix {}

instance intScalarToSelectQuery :: ToSelectQuery (SelectScalar Int) () where
      toSelectQuery (SelectScalar n) suffix = Query (show n) suffix {}

instance selectTupleToSelectQuery :: (ToSelectQuery s parameters, ToSelectQuery t otherParameters, Union parameters otherParameters combined, Nub combined final) => ToSelectQuery (SelectTuple (Tuple s t)) final where
      toSelectQuery (SelectTuple (Tuple s t)) suffix = Query (query <> comma <> otherQuery) otherSuffix $ R.merge parameters otherParameters
            where Query query firstSuffix parameters = toSelectQuery s suffix
                  Query otherQuery otherSuffix otherParameters = toSelectQuery t firstSuffix

--coming from SelectTuple
instance selectToSelectQuery :: ToSelectQuery s parameters => ToSelectQuery (Select s fields) parameters where
      toSelectQuery (Select s) suffix = toSelectQuery s suffix

instance subSelectFromToSelectQuery :: ToFromQuery f parameters => ToSelectQuery (SubSelectFrom f s fields) parameters where
      toSelectQuery (SubSelectFrom fr) suffix = Query (openBracket <> query <> closeBracket) newSuffix parameters
            where Query query newSuffix parameters = toQuery fr suffix

-- instance subSelectWhereToSelectQuery :: ToWhereQuery f => ToSelectQuery (SubSelectWhere f s fields) where
--       toSelectQuery (SubSelectWhere wr) = "(" <> q <> ")"
--             where Query q _ = toQuery wr

--here so From can be a top level
-- actual parsing is done in ToFromQuery
instance fromToQuery :: ToFromQuery f parameters => ToQuery (From f s fields) parameters where
      toQuery (From fr) suffix = toFromQuery fr suffix

class ToFromQuery f parameters | f -> parameters where
      toFromQuery :: f -> Suffix -> Query parameters

instance fromTableToQueryFrom :: (IsSymbol name, ToSelectQuery s parameters) => ToFromQuery (FromTable name (Select s fields) fields) parameters where
      toFromQuery (FromTable s) suffix = Query (query <> fromKeyword <> tableName) newSuffix parameters
            where tableName = DS.reflectSymbol (Proxy :: Proxy name)
                  Query query newSuffix parameters = toQuery s suffix

-- instance fromAsToQueryFrom :: (ToFromQuery f, ToSelectQuery s, ToSelectQuery s2, IsSymbol name) => ToFromQuery (FromAs (As (From f (Select s fields) fields) (Alias name) projection) (Select s2 projection) projection) where
--       toFromQuery (FromAs (As asf) s) = Query q Nothing
--             where Query sel _ = toQuery s
--                   Query aliased _ = toQuery asf
--                   q = sel <> " FROM (" <> aliased <> ") " <> DS.reflectSymbol (Proxy :: Proxy name)

-- instance fromAsWhereToQueryFrom :: (ToWhereQuery f, ToSelectQuery s, ToSelectQuery s2, IsSymbol name) => ToFromQuery (FromAs (As (Where f fields parameters) (Alias name) projection) (Select s projection) projection) where
--       toFromQuery (FromAs (As asf) s) = Query q parameters
--             where Query sel _ = toQuery s
--                   Query aliased parameters = toQuery asf
--                   q = sel <> " FROM (" <> aliased <> ") " <> DS.reflectSymbol (Proxy :: Proxy name)


instance wherToQuery :: ToWhereQuery f subParameters => ToQuery (Where f fields parameters) subParameters where
      toQuery (Where filtered parameters fr) suffix = Query (query <> whereKeyword <> filters) $
            where Query query newSuffix parameters = toWhereQuery fr suffix

                  filters = printFilter filtered
                  printFilter = case _ of
                        Operation field otherField op -> field <> printOperator op <> otherField
                        And filter otherFilter -> "(" <> printFilter filter <> " AND " <> printFilter otherFilter <> ")"
                        Or filter otherFilter -> "(" <> printFilter filter <> " OR " <> printFilter otherFilter <> ")"
                  printOperator = case _ of
                        Equals -> " = "
                        NotEquals -> " <> "

class ToWhereQuery w parameters | w -> parameters where
      toWhereQuery :: w -> Query parameters

instance fromToQueryWhere :: ToFromQuery f parameters => ToWhereQuery (From f s fields) parameters where
      toWhereQuery (From fr) suffix = toFromQuery fr suffix



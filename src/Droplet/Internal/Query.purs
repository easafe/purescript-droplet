-- | This module define `ToQuery`, a type class to generate parameterized SQL statements strings
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet` instead
module Droplet.Internal.Query where

import Droplet.Internal.Language
import Droplet.Internal.Filter
import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))

data Query parameters = Query String (Maybe (Record parameters))

instance queryShow :: Show (Query parameters) where
      show (Query q _) = q

class ToQuery p where
      toQuery :: forall parameters. p parameters -> Query parameters

instance selectPrint :: ToSelectQuery s => ToQuery (Select s) where
      toQuery :: forall fields. Select s fields -> Query fields
      toQuery (Select sel) = Query ("SELECT " <> toSelectQuery sel) Nothing

class ToSelectQuery s where
      toSelectQuery :: s -> String

instance selectFieldPrintSelect :: IsSymbol name => ToSelectQuery (SelectField name fields) where
      toSelectQuery _ = DS.reflectSymbol (Proxy :: Proxy name)

instance tablePrintSelect :: IsSymbol name => ToSelectQuery (SelectTable name fields) where
      toSelectQuery _ = DS.reflectSymbol (Proxy :: Proxy name) <> ".*"

instance subSelectFromPrintSelect :: ToFromQuery f => ToSelectQuery (SubSelectFrom f s fields) where
      toSelectQuery (SubSelectFrom fr) = "(" <> q <> ")"
            where Query q _ = toQuery fr

instance subSelectWherePrintSelect :: ToWhereQuery f => ToSelectQuery (SubSelectWhere f s fields) where
      toSelectQuery (SubSelectWhere wr) = "(" <> q <> ")"
            where Query q _ = toQuery wr

instance intScalarPrintSelect :: ToSelectQuery (SelectScalar Int to) where
      toSelectQuery (SelectScalar n) = show n

instance selectTuplePrintSelect :: (ToSelectQuery s, ToSelectQuery s2) => ToSelectQuery (SelectTuple (Tuple s s2) fields) where
      toSelectQuery (SelectTuple (Tuple s s2)) = toSelectQuery s <> ", " <> toSelectQuery s2

--coming from SelectTuple
instance selectPrintSelect :: ToSelectQuery s => ToSelectQuery (Select s fields) where
      toSelectQuery (Select s) = toSelectQuery s

instance fromPrint :: ToFromQuery f => ToQuery (From f s) where
      toQuery :: forall fields. From f s fields -> Query fields
      toQuery (From fr) = Query (toFromQuery fr) Nothing

class ToFromQuery f where
      toFromQuery :: f -> String

instance fromTablePrintFrom :: (IsSymbol name, ToSelectQuery s) => ToFromQuery (FromTable name (Select s fields) fields) where
      toFromQuery :: FromTable name (Select s fields) fields -> String
      toFromQuery (FromTable s) = sel <> " FROM " <> tableName
            where tableName = DS.reflectSymbol (Proxy :: Proxy name)
                  Query sel _ = toQuery s

instance wherPrint :: ToWhereQuery f => ToQuery (Where f fields) where
      toQuery :: forall parameters. Where f fields parameters -> Query parameters
      toQuery (Where filtered parameters fr) = Query (q <> " WHERE " <> filters) $ Just parameters
            where q = toWhereQuery fr

                  filters = printFilter filtered
                  printFilter = case _ of
                        Operation field otherField op -> field <> printOperator op <> otherField
                        And filter otherFilter -> "(" <> printFilter filter <> " AND " <> printFilter otherFilter <> ")"
                        Or filter otherFilter -> "(" <> printFilter filter <> " OR " <> printFilter otherFilter <> ")"
                  printOperator = case _ of
                        Equals -> " = "
                        NotEquals -> " <> "

class ToWhereQuery f where
      toWhereQuery :: f -> String

instance fromPrintWhere :: ToFromQuery f => ToWhereQuery (From f s fields) where
      toWhereQuery (From fr) = toFromQuery fr

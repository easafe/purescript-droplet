module Droplet where

import Prelude

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.String as DST
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Prim.Row (class Cons, class Union)
import Prim.RowList as RL
import Record.Unsafe as RU
import Type.Proxy (Proxy(..))

{-
select fieldsList ✓ | * ✓ | sub select | function | scalars ✓

from table name ✓ | sub select

where field op field ✓ | field op parameter ✓ | and/or ✓ | sub select

group by fields

order by fields

join right | left
-}

--select

newtype Select s (fields :: Row Type) = Select s

data SelectFields (projection :: Row Type) (fields :: Row Type) = SelectFields

newtype SelectScalar s (fields :: Row Type) = SelectScalar s

--newtype SubSelectFrom (fields :: Row Type) = SubSelectFrom (FromTable (SubSelectFrom fields) fields)

--newtype SubSelectWhere parameters (fields :: Row Type) = SubSelectWhere (Where (FromTable (SubSelectFrom fields) fields) fields parameters)

select :: forall from s to . ToSelect from s to => from -> Select (s to) to
select = toSelect

class ToSelect from s to | from -> s, s -> from where
      toSelect :: from -> Select (s to) to

instance rowToSelect ::
      (RL.RowToList projection fieldsList,
       PrintRowList fieldsList,
       Union projection e fields) =>
      ToSelect (Proxy projection) (SelectFields projection) fields where
      toSelect :: Proxy projection -> Select (SelectFields projection fields) fields
      toSelect _ = Select SelectFields
else
-- instance fromToSelect :: ToSelect (FromTable before fields) SubSelectFrom to where
--       toSelect from = SubSelectFrom from
-- else
-- instance whereToSelect :: ToSelect (Where before fields parameters) (SubSelectWhere parameters) to where
--       toSelect wher = SubSelectWhere wher
-- else
instance showToSelect :: Show s => ToSelect s (SelectScalar s) to where
      toSelect s = Select $ SelectScalar s

--from

newtype From f s (fields :: Row Type) = From f

data Table (name :: Symbol) (fields :: Row Type) = Table

data FromTable (name :: Symbol) s (fields :: Row Type) = FromTable s

table :: forall name fields. IsSymbol name => Table name fields
table = Table

from :: forall from f s to. ToFrom from f to => from -> Select s to -> From (f (Select s to) to) (Select s to) to
from f s = toFrom f s

class ToFrom from f to | from -> f, f -> from where
      toFrom :: forall s. from -> Select s to -> From (f (Select s to) to) (Select s to) to

instance fromTableToFrom :: ToFrom (Table name fields) (FromTable name) fields where
      toFrom :: forall s. Table name fields -> Select s fields -> From (FromTable name (Select s fields) fields) (Select s fields) fields
      toFrom _ s = From $ FromTable s

--where

data Operator =
      Equals |
      NotEquals

newtype Filters (fields :: Row Type) = Filters Filtered

data Filtered =
      Operation String String Operator |
      And Filtered Filtered |
      Or Filtered Filtered

data Where f (fields :: Row Type) parameters = Where Filtered (Record parameters) f

--for whatever reason, using the proxy parameters instead of the types generates an error in the Data.Symbol module
equals :: forall fields e extra field1 field2 t.
      IsSymbol field1 =>
      IsSymbol field2 =>
      Cons field1 t e extra =>
      Cons field2 t extra fields =>
      Proxy field1 -> Proxy field2 -> Filters fields
equals _ _ = Filters $ Operation (DS.reflectSymbol (Proxy :: Proxy field1)) (DS.reflectSymbol (Proxy :: Proxy field2)) Equals

notEquals :: forall fields e extra field1 field2 t.
      IsSymbol field1 =>
      IsSymbol field2 =>
      Cons field1 t e extra =>
      Cons field2 t extra fields =>
      Proxy field1 -> Proxy field2 -> Filters fields
notEquals _ _ = Filters $ Operation (DS.reflectSymbol (Proxy :: Proxy field1)) (DS.reflectSymbol (Proxy :: Proxy field2)) NotEquals

and :: forall fields. Filters fields -> Filters fields -> Filters fields
and (Filters first) (Filters second) = Filters (And first second)

or :: forall fields. Filters fields -> Filters fields -> Filters fields
or (Filters first) (Filters second) = Filters (Or first second)

infix 4 notEquals as .<>.
infix 4 equals as .=.
infixr 3 and as .&&.
infixr 2 or as .||.

--it should be a type error for the field list and parameter list to share fields!
wher :: forall f s fields parameters all.
      Union fields parameters all =>
      Filters all -> Record parameters -> From f s fields -> Where (From f s fields) fields parameters
wher (Filters filtered) parameters before = Where filtered parameters before

--print

data Query parameters = Query String (Maybe (Record parameters))

class Print p where
      print :: forall parameters. p parameters -> Query parameters

instance selectPrint :: PrintSelect s => Print (Select s) where
      print :: forall fields. Select s fields -> Query fields
      print (Select sel) = Query (printSelect sel) Nothing

class PrintSelect s where
      printSelect :: s -> String

instance selectFieldPrintSelect ::
      (RL.RowToList projection fieldsList,
       PrintRowList fieldsList,
       Union projection e fields) =>
      PrintSelect (SelectFields projection fields) where
      printSelect _ = "SELECT " <> fields
            where fields = DST.joinWith ", " $ printRowList (Proxy :: Proxy fieldsList)

instance showPrintSelect :: Show s => PrintSelect (SelectScalar s to) where
      printSelect (SelectScalar s) = "SELECT " <> show s

class PrintRowList (fieldsList :: RL.RowList Type) where
      printRowList :: forall proxy. proxy fieldsList -> Array String

instance nilPrintRowList :: PrintRowList RL.Nil where
      printRowList _ = []

instance consPrintRowList :: (PrintRowList tail, IsSymbol field) => PrintRowList (RL.Cons field v tail) where
      printRowList _ = DA.snoc (printRowList (Proxy :: Proxy tail)) $ DS.reflectSymbol (Proxy :: Proxy field)

instance fromPrint :: PrintFrom f => Print (From f s) where
      print :: forall fields. From f s fields -> Query fields
      print (From fr) = Query (printFrom fr) Nothing

class PrintFrom f where
      printFrom :: f -> String

instance fromTablePrintFrom :: (IsSymbol name, PrintSelect s) => PrintFrom (FromTable name (Select s fields) fields) where
      printFrom :: FromTable name (Select s fields) fields -> String
      printFrom (FromTable (Select s)) = sel <> " FROM " <> tableName
            where tableName = DS.reflectSymbol (Proxy :: Proxy name)
                  sel = printSelect s

-- instance wherPrint :: Print f => Print (Where f fields) where
--       print :: forall parameters. Where f fields parameters -> Query parameters
--       print (Where filtered parameters fr) = Query (q <> " WHERE " <> filters) $ Just parameters
--             where Query q _ = print fr

--                   filters = printFilter filtered
--                   printFilter = case _ of
--                         Operation field otherField op ->
--                               fieldOrParameter field <> printOperator op <> fieldOrParameter otherField
--                         And filter otherFilter -> printFilter filter <> " AND " <> printFilter otherFilter
--                         Or filter otherFilter -> printFilter filter <> " OR " <> printFilter otherFilter
--                   fieldOrParameter field
--                         | RU.unsafeHas field parameters = "@" <> field
--                         | otherwise = field
--                   printOperator = case _ of
--                         Equals -> " = "
--                         NotEquals -> " <> "

instance queryShow :: Show (Query parameters) where
      show (Query q _) = q
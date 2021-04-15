module Droplet where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Union)
import Record.Unsafe as RU
import Type.Proxy (Proxy(..))

--do the api improvements first (in where only now)
-- then finish sub selects for from and where

{-
select fieldsList ✓ | * ✓ | sub select ✓ | function | scalars ✓ | column names

limit

from table name ✓ | sub select and table names

where field op field ✓ | field op parameter ✓ | and/or ✓ | sub select (maybe only exists?)

group by fields

order by fields

join right | left
-}

--select

--I can't seem to make this anything like a gadt :(
newtype Select s (fields :: Row Type) = Select s

data SelectField (field :: Symbol) (fields :: Row Type) = SelectField

data SelectTable (name :: Symbol) (fields :: Row Type) = SelectTable

newtype SelectScalar s (fields :: Row Type) = SelectScalar s

newtype SelectTuple s (fields :: Row Type) = SelectTuple s

newtype SubSelectFrom f s (fields :: Row Type) = SubSelectFrom (From f s fields)

newtype SubSelectWhere f (fields :: Row Type) parameters = SubSelectWhere (Where f fields parameters)

data Field (name :: Symbol) = Field

select :: forall from s to . IsSelectable from => ToSelect from s to => from -> Select s to
select = toSelect

--to catch ill typed selects soon
class IsSelectable from

instance fieldIsSelectable :: IsSelectable (Field name)
instance intIsSelectable :: IsSelectable Int
instance tableIsSelectable :: IsSelectable (Table name fields)
instance tupleIsSelectable :: (IsSelectable from, IsSelectable from2) => IsSelectable (Tuple from from2)
instance fromIsSelectable :: IsSelectable (From f (Select s fields) fields)
instance whereIsSelectable :: IsSelectable (Where (From f (Select s fields) fields) fields parameters)

class ToSelect from s to | from -> s, s -> from where
      toSelect :: from -> Select s to

--as it is, we can't express select table.* /\ table2.*
-- nor sub queries without from (which I dont know if it is ever useful)
instance fieldToSelect :: Cons name t e fields => ToSelect (Field name) (SelectField name fields) fields where
      toSelect _ = Select SelectField
else
instance tableToSelect :: ToSelect (Table name fields) (SelectTable name fields) fields where
      toSelect _ = Select SelectTable
else
instance fromToSelect :: ToSubSelect from s to => ToSelect (From f (Select s to) to) (SubSelectFrom f (Select s to) to) fields where
      toSelect fr = Select $ SubSelectFrom fr
else
instance whereToSelect :: ToSubSelect from s to => ToSelect (Where (From f (Select s to) to) to parameters) (SubSelectWhere (From f (Select s to) to) to parameters) fields where
      toSelect wr = Select $ SubSelectWhere wr
else
instance tupleToSelect ::
      (ToSelect from s fields,
       ToSelect from2 s2 fields) =>
      ToSelect (Tuple from from2) (SelectTuple (Tuple (Select s fields) (Select s2 fields)) fields) fields where
      toSelect (Tuple t t2) = Select <<< SelectTuple <<< Tuple (toSelect t) $ toSelect t2
else
instance intToSelect :: ToSelect Int (SelectScalar Int to) to where
      toSelect n = Select $ SelectScalar n
--needs more instance for scalars

--for sub queries only a single column can be returned
class ToSubSelect from s to | from -> s, s -> from where
      toSubSelect :: from -> Select s to

instance rowToSubSelect :: Cons name t e fields => ToSubSelect (Field name) (SelectField name fields) fields where
      toSubSelect _ = Select SelectField
else
instance intToSubSelect :: ToSubSelect Int (SelectScalar Int to) to where
      toSubSelect n = Select $ SelectScalar n
--needs more instance for scalars

--from

newtype From f s (fields :: Row Type) = From f

data Table (name :: Symbol) (fields :: Row Type) = Table

newtype FromTable (name :: Symbol) s (fields :: Row Type) = FromTable s

from :: forall from f s to. IsFromable from => ToFrom from f to => from -> Select s to -> From (f (Select s to) to) (Select s to) to
from f s = toFrom f s

--to catch ill typed froms soon
class IsFromable from

instance fieldIsFromable :: IsFromable (Table name fields)

class ToFrom from f to | from -> f, f -> from where
      toFrom :: forall s. from -> Select s to -> From (f (Select s to) to) (Select s to) to

instance fromTableToFrom :: ToFrom (Table name fields) (FromTable name) fields where
      toFrom :: forall s. Table name fields -> Select s fields -> From (FromTable name (Select s fields) fields) (Select s fields) fields
      toFrom _ s = From $ FromTable s

--where
data Operator =
      Equals |
      NotEquals

newtype Filters (fields :: Row Type) (parameters :: Row Type) = Filters Filtered

data Parameter (name :: Symbol) = Parameter

data Filtered =
      Operation String String Operator |
      And Filtered Filtered |
      Or Filtered Filtered

data Where f (fields :: Row Type) parameters = Where Filtered (Record parameters) f

--for whatever reason, using the proxy parameters instead of the types generates an error in the Data.Symbol module
equals :: forall parameters fields field compared.
      ToCompared field fields parameters =>
      ToCompared compared fields parameters =>
      field -> compared -> Filters fields parameters
equals field compared = Filters $ Operation (toCompared field) (toCompared compared) Equals

notEquals :: forall parameters fields field compared.
      ToCompared field fields parameters =>
      ToCompared compared fields parameters =>
      field -> compared -> Filters fields parameters
notEquals field compared = Filters $ Operation (toCompared field) (toCompared compared) NotEquals

and :: forall fields parameters. Filters fields parameters -> Filters fields parameters -> Filters fields parameters
and (Filters first) (Filters second) = Filters (And first second)

or :: forall fields parameters. Filters fields parameters -> Filters fields parameters -> Filters fields parameters
or (Filters first) (Filters second) = Filters (Or first second)

infix 4 notEquals as .<>.
infix 4 equals as .=.
--left associativity is what sql uses
infixl 3 and as .&&.
infixl 2 or as .||.

wher :: forall f s fields parameters. Filters fields parameters -> Record parameters -> From f s fields -> Where (From f s fields) fields parameters
wher (Filters filtered) parameters before = Where filtered parameters before

--it d be nicer if field parsing was entirely in Print....
class ToCompared c fields parameters | c -> fields, c -> parameters  where
      toCompared :: c -> String

instance fieldToCompared :: (IsSymbol name, Cons name t e fields) => ToCompared (Field name) fields parameters where
      toCompared _ = DS.reflectSymbol (Proxy :: Proxy name)

instance parameterToCompared :: (IsSymbol name, Cons name t e parameters) => ToCompared (Parameter name) fields parameters where
      toCompared _ = "@" <> DS.reflectSymbol (Proxy :: Proxy name)

--print

data Query parameters = Query String (Maybe (Record parameters))

class Print p where
      print :: forall parameters. p parameters -> Query parameters

instance selectPrint :: PrintSelect s => Print (Select s) where
      print :: forall fields. Select s fields -> Query fields
      print (Select sel) = Query ("SELECT " <> printSelect sel ) Nothing

class PrintSelect s where
      printSelect :: s -> String

instance selectFieldPrintSelect :: IsSymbol name => PrintSelect (SelectField name fields) where
      printSelect _ = DS.reflectSymbol (Proxy :: Proxy name)

instance tablePrintSelect :: IsSymbol name => PrintSelect (SelectTable name fields) where
      printSelect _ = DS.reflectSymbol (Proxy :: Proxy name) <> ".*"

instance subSelectFromPrintSelect :: PrintFrom f => PrintSelect (SubSelectFrom f s fields) where
      printSelect (SubSelectFrom fr) = "(" <> q <> ")"
            where Query q _ = print fr

instance subSelectWherePrintSelect :: PrintWhere f => PrintSelect (SubSelectWhere f s fields) where
      printSelect (SubSelectWhere wr) = "(" <> q <> ")"
            where Query q _ = print wr

instance intScalarPrintSelect :: PrintSelect (SelectScalar Int to) where
      printSelect (SelectScalar n) = show n

instance selectTuplePrintSelect :: (PrintSelect s, PrintSelect s2) => PrintSelect (SelectTuple (Tuple s s2) fields) where
      printSelect (SelectTuple (Tuple s s2)) = printSelect s <> ", " <> printSelect s2

--coming from SelectTuple
instance selectPrintSelect :: PrintSelect s => PrintSelect (Select s fields) where
      printSelect (Select s) = printSelect s

instance fromPrint :: PrintFrom f => Print (From f s) where
      print :: forall fields. From f s fields -> Query fields
      print (From fr) = Query (printFrom fr) Nothing

class PrintFrom f where
      printFrom :: f -> String

instance fromTablePrintFrom :: (IsSymbol name, PrintSelect s) => PrintFrom (FromTable name (Select s fields) fields) where
      printFrom :: FromTable name (Select s fields) fields -> String
      printFrom (FromTable s) = sel <> " FROM " <> tableName
            where tableName = DS.reflectSymbol (Proxy :: Proxy name)
                  Query sel _ = print s

instance wherPrint :: PrintWhere f => Print (Where f fields) where
      print :: forall parameters. Where f fields parameters -> Query parameters
      print (Where filtered parameters fr) = Query (q <> " WHERE " <> filters) $ Just parameters
            where q = printWhere fr

                  filters = printFilter filtered
                  printFilter = case _ of
                        Operation field otherField op -> field <> printOperator op <> otherField
                        And filter otherFilter -> "(" <> printFilter filter <> " AND " <> printFilter otherFilter <> ")"
                        Or filter otherFilter -> "(" <> printFilter filter <> " OR " <> printFilter otherFilter <> ")"
                  printOperator = case _ of
                        Equals -> " = "
                        NotEquals -> " <> "

class PrintWhere f where
      printWhere :: f -> String

instance fromPrintWhere :: PrintFrom f => PrintWhere (From f s fields) where
      printWhere (From fr) = printFrom fr

instance queryShow :: Show (Query parameters) where
      show (Query q _) = q
module Droplet where

import Prelude

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.String as DST
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Union)
import Prim.RowList as RL
import Record.Unsafe as RU
import Type.Proxy (Proxy(..))

{-
select fieldsList ✓ | * ✓ | sub select | function | scalars ✓ | column names

limit

from table name ✓ | sub select | table names

where field op field ✓ | field op parameter ✓ | and/or ✓ | sub select

group by fields

order by fields

join right | left
-}

--select

--it is prolly a better api to only accept Field, Table or tuples
newtype Select s (fields :: Row Type) = Select s

data SelectFields (projection :: Row Type) (fields :: Row Type) = SelectFields

newtype SelectScalar s (fields :: Row Type) = SelectScalar s

newtype SelectTuple s (fields :: Row Type) = SelectTuple s

newtype SubSelectFrom f s (fields :: Row Type) = SubSelectFrom (From f s fields)

newtype SubSelectWhere f (fields :: Row Type) parameters = SubSelectWhere (Where f fields parameters)

data Field (name :: Symbol) = Field

select :: forall from s to . ToSelect from s to => from -> Select s to
select = toSelect

class ToSelect from s to | from -> s, s -> from where
      toSelect :: from -> Select s to

instance rowToSelect ::
      (RL.RowToList projection fieldsList,
       PrintRowList fieldsList,
       Union projection e fields) =>
      ToSelect (Proxy projection) (SelectFields projection fields) fields where
      toSelect :: Proxy projection -> Select (SelectFields projection fields) fields
      toSelect _ = Select SelectFields
else
instance fieldToSelect :: (Cons name t () projection, Cons name t e2 fields) => ToSelect (Field name) (SelectFields projection fields) fields where
      toSelect _ = Select SelectFields
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
instance showToSelect :: Show s => ToSelect s (SelectScalar s to) to where
      toSelect s = Select $ SelectScalar s

--for sub queries only a single column can be returned
class ToSubSelect from s to | from -> s, s -> from where
      toSubSelect :: from -> Select s to

instance rowToSubSelect ::
      (RL.RowToList column fieldsList,
       ToSubSelectColumn fieldsList,
       Union column e fields) =>
      ToSubSelect (Proxy column) (SelectFields column fields) fields where
      toSubSelect :: Proxy column -> Select (SelectFields column fields) fields
      toSubSelect _ = Select SelectFields
else
instance showToSubSelect :: Show s => ToSubSelect s (SelectScalar s to) to where
      toSubSelect s = Select $ SelectScalar s

class ToSubSelectColumn (fieldsList :: RL.RowList Type)

instance consToSubSelectColumn :: IsSymbol field => ToSubSelectColumn (RL.Cons field t RL.Nil)

--from

newtype From f s (fields :: Row Type) = From f

data Table (name :: Symbol) (fields :: Row Type) = Table

newtype FromTable (name :: Symbol) s (fields :: Row Type) = FromTable s

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

--WHERE HAS TO ACCEPT FIELD NOT PROXY
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
      print (Select sel) = Query ("SELECT " <> printSelect sel ) Nothing

class PrintSelect s where
      printSelect :: s -> String

instance selectFieldPrintSelect ::
      (RL.RowToList projection fieldsList,
       PrintRowList fieldsList,
       Union projection e fields) =>
      PrintSelect (SelectFields projection fields) where
      printSelect _ = fields
            where fields = DST.joinWith ", " $ printRowList (Proxy :: Proxy fieldsList)

instance subSelectFromPrintSelect :: PrintFrom f => PrintSelect (SubSelectFrom f s fields) where
      printSelect (SubSelectFrom fr) = "(" <> q <> ")"
            where Query q _ = print fr

instance showPrintSelect :: Show s => PrintSelect (SelectScalar s to) where
      printSelect (SelectScalar s) = show s

instance selectTuplePrintSelect :: (PrintSelect s, PrintSelect s2) => PrintSelect (SelectTuple (Tuple s s2) fields) where
      printSelect (SelectTuple (Tuple s s2)) = printSelect s <> ", " <> printSelect s2

--coming from SelectTuple
instance selectPrintSelect :: PrintSelect s => PrintSelect (Select s fields) where
      printSelect (Select s) = printSelect s

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
      printFrom (FromTable s) = sel <> " FROM " <> tableName
            where tableName = DS.reflectSymbol (Proxy :: Proxy name)
                  Query sel _ = print s

instance wherPrint :: PrintWhere f => Print (Where f fields) where
      print :: forall parameters. Where f fields parameters -> Query parameters
      print (Where filtered parameters fr) = Query (q <> " WHERE " <> filters) $ Just parameters
            where q = printWhere fr

                  filters = printFilter filtered
                  printFilter = case _ of
                        Operation field otherField op ->
                              fieldOrParameter field <> printOperator op <> fieldOrParameter otherField
                        And filter otherFilter -> printFilter filter <> " AND " <> printFilter otherFilter
                        Or filter otherFilter -> printFilter filter <> " OR " <> printFilter otherFilter
                  fieldOrParameter field
                        | RU.unsafeHas field parameters = "@" <> field
                        | otherwise = field
                  printOperator = case _ of
                        Equals -> " = "
                        NotEquals -> " <> "

class PrintWhere f where
      printWhere :: f -> String

instance fromPrintWhere :: PrintFrom f => PrintWhere (From f s fields) where
      printWhere (From fr) = printFrom fr

instance queryShow :: Show (Query parameters) where
      show (Query q _) = q
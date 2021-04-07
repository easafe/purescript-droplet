module Droplet where

import Prelude

import Data.Array as DA
import Data.Foldable as DF
import Data.String as DST
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Prim.Row (class Cons, class Union)
import Prim.RowList as RL
import Record.Unsafe as RU
import Type.Proxy (Proxy(..))

{-
select fieldsList ✓ | * ✓ | sub select | function | scalars | compose

from table name ✓ | sub select | compose

where field op field ✓ | field op parameter ✓ | and/or ✓ | compose

join right | left | compose
-}

--select

data Select (fields :: Row Type) parameters = Select String (From fields) (Where fields parameters)

select :: forall projection e fields fieldsList parameters.
      Union projection e fields =>
      RL.RowToList projection fieldsList =>
      RowSelect fieldsList =>
      Proxy projection -> From fields -> Where fields parameters -> Select fields parameters
select _ froms wheres = Select fieldsList froms wheres
      --only works for this kind of projection!
      where fieldsList = DST.joinWith ", " $ toRowFieldList (Proxy :: Proxy fieldsList)

class RowSelect (fieldsList :: RL.RowList Type) where
      toRowFieldList :: forall proxy. proxy fieldsList -> Array String

instance nilRowSelect :: RowSelect RL.Nil where
      toRowFieldList _ = []

instance consRowSelectable :: (RowSelect tail, IsSymbol field) => RowSelect (RL.Cons field v tail) where
      toRowFieldList _ = DA.snoc (toRowFieldList (Proxy :: Proxy tail)) $ DS.reflectSymbol (Proxy :: Proxy field)

--from

data Table (fieldsList :: Row Type) (name :: Symbol) = Table

data From (fieldsList :: Row Type) = FromTable String

table :: forall fields name. IsSymbol name => Table fields name
table = Table

from :: forall fields name. IsSymbol name => Table fields name -> From fields
from _ = FromTable $ DS.reflectSymbol (Proxy :: Proxy name)

--where

data Operator = Equals

newtype Filters (fields :: Row Type) = Filters Filtered

data Filtered =
      Operation String String Operator |
      And Filtered Filtered

data Where (fields :: Row Type) parameters = Where Filtered (Record parameters)

--should make these operators so can avoid brackets
--for whatever reason, using the parameters instead of the types generates an error in the Data.Symbol module
equals :: forall fields e extra field1 field2 t.
      IsSymbol field1 =>
      IsSymbol field2 =>
      Cons field1 t e extra =>
      Cons field2 t extra fields =>
      Proxy field1 -> Proxy field2 -> Filters fields
equals _ _ = Filters $ Operation (DS.reflectSymbol (Proxy :: Proxy field1)) (DS.reflectSymbol (Proxy :: Proxy field2)) Equals

--need to make sure that these cant generate invalid sql
and :: forall fields. Filters fields -> Filters fields -> Filters fields
and (Filters first) (Filters second) = Filters (And first second)

--it should be a type error for the field list and parameter list to share fields!
wher :: forall fields parameters all. Union fields parameters all => Filters all -> Record parameters -> Where fields parameters
wher (Filters filtered) parameters = Where filtered parameters

--print

data Query parameters = Query String (Record parameters)

print :: forall fields parameters. Select fields parameters -> Query parameters
print (Select fieldsList (FromTable tableName) (Where filtered parameters)) = Query query parameters
      where query = "SELECT " <> fieldsList <> " FROM " <> tableName <> " WHERE " <> filters

            filters = printFilter filtered
            printFilter = case _ of
                  Operation field otherField op ->
                        fieldOrParameter field <> printOperator op <> fieldOrParameter otherField
                  And filter otherFilter -> printFilter filter <> " AND " <> printFilter otherFilter
            fieldOrParameter field
                  | RU.unsafeHas field parameters = "@" <> field
                  | otherwise = field
            printOperator = case _ of
                  Equals -> " = "

instance queryShow :: Show (Query parameters) where
      show (Query q _) = q

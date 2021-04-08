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

data Select (fields :: Row Type) = Select String

select :: forall s to . ToSelect s to => s -> Select to
select = toSelect

class ToSelect from to | from -> to where
      toSelect :: from -> Select to

instance intToSelect :: ToSelect Int to where
      toSelect n = Select $ show n

instance rowToSelect ::
      (RL.RowToList projection fieldsList,
       RowListSelect fieldsList,
       Union projection e fields) =>
      ToSelect (Proxy projection) fields where
      toSelect :: Proxy projection -> Select fields
      toSelect _ = Select fieldsList
            where fieldsList = DST.joinWith ", " $ toRowFieldList (Proxy :: Proxy fieldsList)

class RowListSelect (fieldsList :: RL.RowList Type) where
      toRowFieldList :: forall proxy. proxy fieldsList -> Array String

instance nilRowSelect :: RowListSelect RL.Nil where
      toRowFieldList _ = []

instance consRowSelectable :: (RowListSelect tail, IsSymbol field) => RowListSelect (RL.Cons field v tail) where
      toRowFieldList _ = DA.snoc (toRowFieldList (Proxy :: Proxy tail)) $ DS.reflectSymbol (Proxy :: Proxy field)

--from

data Table (fields :: Row Type) (name :: Symbol) = Table

data From (fields :: Row Type) = FromTable String (Select fields)

table :: forall fields name. IsSymbol name => Table fields name
table = Table

from :: forall fields name . IsSymbol name => Table fields name -> Select fields -> From fields
from _ before = FromTable (DS.reflectSymbol (Proxy :: Proxy name)) before

--where

data Operator =
      Equals |
      NotEquals

newtype Filters (fields :: Row Type) = Filters Filtered

data Filtered =
      Operation String String Operator |
      And Filtered Filtered |
      Or Filtered Filtered

data Where (fields :: Row Type) parameters = Where Filtered (Record parameters) (From fields)

--for whatever reason, using the parameters instead of the types generates an error in the Data.Symbol module
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
wher :: forall fields parameters all.
      Union fields parameters all =>
      Filters all -> Record parameters -> From fields -> Where fields parameters
wher (Filters filtered) parameters before = Where filtered parameters before

--print

data Query parameters = Query String (Maybe (Record parameters))

class Print p where
      print :: forall parameters. p parameters -> Query parameters

instance selectPrint :: Print Select where
      print :: forall fields. Select fields -> Query fields
      print (Select s) = Query ("SELECT " <> s) Nothing

instance fromPrint :: Print From where
      print :: forall fields. From fields -> Query fields
      print (FromTable f before) = Query (q <> " FROM " <> f) Nothing
            where Query q _ = print before

instance wherPrint :: Print (Where fields) where
      print :: forall parameters. Where fields parameters -> Query parameters
      print (Where filtered parameters before) = Query (q <> " WHERE " <> filters) $ Just parameters
            where Query q _ = print before

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

instance queryShow :: Show (Query parameters) where
      show (Query q _) = q
module Droplet where

import Prelude

import Data.Array as DA
import Data.String as DST
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Prim.Row (class Cons, class Union)
import Prim.RowList as RL
import Record.Unsafe as RU
import Type.Proxy (Proxy(..))

{-
select fieldsList ✓ | * ✓ | sub select | function | scalars

from table name ✓ | sub select

where field op field ✓ | field op parameter ✓ | and/or ✓

join right | left

-}

--select

data Query parameters = Query (Record parameters) String

data Select (fields :: Row Type) (name :: Symbol) parameters = Select String (From fields name) (Where parameters fields)

class RowSelect (fieldsList :: RL.RowList Type) where
      toRowFieldList :: forall proxy. proxy fieldsList -> Array String

instance nilRowSelect :: RowSelect RL.Nil where
      toRowFieldList _ = []

instance consRowSelectable :: (RowSelect tail, IsSymbol field) => RowSelect (RL.Cons field v tail) where
      toRowFieldList _ = DA.snoc (toRowFieldList (Proxy :: Proxy tail)) $ DS.reflectSymbol (Proxy :: Proxy field)

select :: forall projection extra fields fieldsList name parameters.
      Union projection extra fields =>
      RL.RowToList projection fieldsList =>
      RowSelect fieldsList =>
      IsSymbol name => Proxy projection -> From fields name -> Where fields parameters -> Query parameters
select _ (FromTable table) (Where parameters filter) = Query parameters $ "SELECT " <> fieldsList <> " FROM " <> show table <> " " <> filter
      where fieldsList = DST.joinWith ", " $ toRowFieldList (Proxy :: Proxy fieldsList)

--from

data Table (fieldsList :: Row Type) (name :: Symbol) = Table

table :: forall fields fieldsList name. RL.RowToList fields fieldsList => RowSelect fieldsList => IsSymbol name => Table fields name
table = Table

instance tableShow :: IsSymbol name => Show (Table fields name) where
      show _ = DS.reflectSymbol (Proxy :: Proxy name)

instance tableQuery :: Show (Query parameters) where
      show (Query _ query) = query

data From (fieldsList :: Row Type) (name :: Symbol) = FromTable (Table fieldsList name)

from :: forall fields fieldsList name. RL.RowToList fields fieldsList => IsSymbol name => RowSelect fieldsList => Table fields name -> From fields name
from table = FromTable table

--where

data Operator = Equals

instance operatorShow  :: Show Operator where
      show = case _ of
            Equals -> " = "

data Filters (fields :: Row Type) =
      Operation String String Operator |
      And (Filters fields) (Filters fields)

--should make these operators so can avoid brackets

--for whatever reason, using the parameters instead of the types generates an error in the Data.Symbol module
equals :: forall fields e extra field1 field2 t.
      IsSymbol field1 =>
      IsSymbol field2 =>
      Cons field1 t e extra =>
      Cons field2 t extra fields =>
      Proxy field1 -> Proxy field2 -> Filters fields
equals _ _ = Operation (DS.reflectSymbol (Proxy :: Proxy field1)) (DS.reflectSymbol (Proxy :: Proxy field2)) Equals

--need to make sure that these cant generate invalid sql
and :: forall fields. Filters fields -> Filters fields -> Filters fields
and first second = And first second

data Where (fields :: Row Type) parameters = Where (Record parameters) String

--it should be a type error for the field list and parameter list to share fields!
wher :: forall fields parameters all. Union fields parameters all => Filters all -> Record parameters -> Where fields parameters
wher filters parameters = Where parameters $ "WHERE " <> print filters -- this is a fold ....
      where print = case _ of
                  Operation field otherField op ->
                        fieldOrParameter field <> show op <> fieldOrParameter otherField
                  And filter otherFilter -> print filter <> " AND " <> print otherFilter

            fieldOrParameter field
                  | RU.unsafeHas field parameters = "@" <> field
                  | otherwise = field
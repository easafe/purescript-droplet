module Droplet where

import Prelude

import Data.Array as DA
import Data.String as DST
import Data.Symbol (class IsSymbol)
import Data.Symbol as DS
import Prim.Row (class Cons, class Union)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

{-
select fieldsList ✓ | * ✓ | function | scalars

from table name ✓ | sub select | select without table

where field op field ✓ | field op parameter | and/or ✓

-}

--select

newtype Query = Query String

data Select (fields :: Row Type) (name :: Symbol) = Select String (From fields name) (Where fields)

class RowSelect (fieldsList :: RL.RowList Type) where
      toRowFieldList :: forall proxy. proxy fieldsList -> Array String

instance nilRowSelect :: RowSelect RL.Nil where
      toRowFieldList _ = []

instance consRowSelectable :: (RowSelect tail, IsSymbol field) => RowSelect (RL.Cons field v tail) where
      toRowFieldList _ = DA.snoc (toRowFieldList (Proxy :: Proxy tail)) $ DS.reflectSymbol (Proxy :: Proxy field)

select :: forall projection extra fields fieldsList name.
      Union projection extra fields =>
      RL.RowToList projection fieldsList =>
      RowSelect fieldsList =>
      IsSymbol name => Proxy projection -> From fields name -> Where fields -> Query
select _ from = toQuery <<< Select fieldsList from
      where fieldsList = DST.joinWith ", " $ toRowFieldList (Proxy :: Proxy fieldsList)
            toQuery = case _ of
                  Select fields (FromTable table) (Where filter) -> Query $ "SELECT " <> fields <> " FROM " <> show table <> " " <> filter

--from

data Table (fieldsList :: Row Type) (name :: Symbol) = Table

table :: forall fields fieldsList name. RL.RowToList fields fieldsList => RowSelect fieldsList => IsSymbol name => Table fields name
table = Table

instance tableShow :: IsSymbol name => Show (Table fields name) where
      show _ = DS.reflectSymbol (Proxy :: Proxy name)

instance tableQuery :: Show Query where
      show (Query query) = query

data From (fieldsList :: Row Type) (name :: Symbol) = FromTable (Table fieldsList name)

from :: forall fields fieldsList name. RL.RowToList fields fieldsList => IsSymbol name => RowSelect fieldsList => Table fields name -> From fields name
from table = FromTable table

--where

newtype Operation (fields :: Row Type) = Operation String

--should make these operators so can avoid brackets

--for whatever reason, using the parameters instead of the types generates an error in the Data.Symbol module
equals :: forall fields e extra field1 field2 t.
      IsSymbol field1 =>
      IsSymbol field2 =>
      Cons field1 t e extra =>
      Cons field2 t extra fields =>
      Proxy field1 -> Proxy field2 -> Operation fields
equals _ _ = Operation $ ((DS.reflectSymbol (Proxy :: Proxy field1)) <> " = " <> (DS.reflectSymbol (Proxy :: Proxy field2)))

--need to make sure that these cant generate invalid sql
and :: forall fields. Operation fields -> Operation fields -> Operation fields
and (Operation first) (Operation second) = Operation $ first <> " AND " <> second

newtype Where (fields :: Row Type) = Where String

wher :: forall fields parameters fieldsList. RL.RowToList (fields + parameters) fieldsList => RowSelect fieldsList => Operation (fields + parameters) -> Where (fields + parameters)
wher (Operation op) = Where $ "WHERE " <> op



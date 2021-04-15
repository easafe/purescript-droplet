-- | This module defines the entire SQL EDSL, mostly because it'd be a pain to split it
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet` instead
module Droplet.Internal.Language where

import Prelude

import Data.Tuple (Tuple(..))
import Prim.Row (class Cons)
import Droplet.Internal.Filter
import Droplet.Internal.Definition

----------------------SELECT----------------------------

--I can't seem to make this anything like a gadt :(
newtype Select s (fields :: Row Type) = Select s

data SelectField (field :: Symbol) (fields :: Row Type) = SelectField

data SelectTable (name :: Symbol) (fields :: Row Type) = SelectTable

newtype SelectScalar s (fields :: Row Type) = SelectScalar s

newtype SelectTuple s (fields :: Row Type) = SelectTuple s

newtype SubSelectFrom :: forall k. Type -> k -> Row Type -> Type
newtype SubSelectFrom f s (fields :: Row Type) = SubSelectFrom (From f s fields)

newtype SubSelectWhere f (fields :: Row Type) parameters = SubSelectWhere (Where f fields parameters)

select :: forall from s to . IsSelectable from => ToSelect from s to => from -> Select s to
select = toSelect

--to catch ill typed selects soon
class IsSelectable :: forall k. k -> Constraint
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

----------------------------FROM----------------------------

newtype From :: forall k. Type -> k -> Row Type -> Type
newtype From f s (fields :: Row Type) = From f

newtype FromTable (name :: Symbol) s (fields :: Row Type) = FromTable s

from :: forall from f s to. IsFromable from => ToFrom from f to => from -> Select s to -> From (f (Select s to) to) (Select s to) to
from f s = toFrom f s

--to catch ill typed froms soon
class IsFromable :: forall k. k -> Constraint
class IsFromable from

instance fieldIsFromable :: IsFromable (Table name fields)

class ToFrom from f to | from -> f, f -> from where
      toFrom :: forall s. from -> Select s to -> From (f (Select s to) to) (Select s to) to

instance fromTableToFrom :: ToFrom (Table name fields) (FromTable name) fields where
      toFrom :: forall s. Table name fields -> Select s fields -> From (FromTable name (Select s fields) fields) (Select s fields) fields
      toFrom _ s = From $ FromTable s

----------------------------WHERE----------------------------

data Where f (fields :: Row Type) parameters = Where Filtered (Record parameters) f

wher :: forall f s fields parameters. Filters fields parameters -> Record parameters -> From f s fields -> Where (From f s fields) fields parameters
wher (Filters filtered) parameters before = Where filtered parameters before

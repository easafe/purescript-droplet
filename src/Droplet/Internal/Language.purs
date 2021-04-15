-- | This module defines the entire SQL EDSL, mostly because it'd be a pain to split it
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet` instead
module Droplet.Internal.Language where

import Droplet.Internal.Definition
import Droplet.Internal.Filter
import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Union)



----------------------SELECT----------------------------

--I can't seem to make this anything like a gadt :(
--fields should be a row list of everything selected?
newtype Select s (fields :: Row Type) = Select s

data SelectField (field :: Symbol) = SelectField

data SelectTable (name :: Symbol) = SelectTable

newtype SelectScalar s = SelectScalar s

newtype SelectTuple s = SelectTuple s

newtype SubSelectFrom :: forall k. Type -> k -> Row Type -> Type
newtype SubSelectFrom f s (fields :: Row Type) = SubSelectFrom (From f s fields)

newtype SubSelectWhere f (fields :: Row Type) parameters = SubSelectWhere (Where f fields parameters)

class ToSelect from s fields | from -> s, s -> from where
      toSelect :: from -> Select s fields

--as it is, we can't express select table.* /\ table2.*
-- nor sub queries without from (which I dont know if it is ever useful)
instance fieldToSelect :: Cons name t e fields => ToSelect (Field name) (SelectField name) fields where
      toSelect _ = Select SelectField
else
instance tableToSelect :: ToSelect (Table name fields) (SelectTable name) fields where
      toSelect _ = Select SelectTable
else
instance fromToSelect :: ToSubSelect from s to => ToSelect (From f (Select s to) to) (SubSelectFrom f (Select s to) to) fields where
      toSelect fr = Select $ SubSelectFrom fr
else
instance whereToSelect :: ToSubSelect from s to => ToSelect (Where (From f (Select s to) to) to parameters) (SubSelectWhere (From f (Select s to) to) to parameters) fields where
      toSelect wr = Select $ SubSelectWhere wr
else
instance tupleToSelect :: (ToSelect from s fields, ToSelect from2 s2 fields) => ToSelect (Tuple from from2) (SelectTuple (Tuple (Select s fields) (Select s2 fields))) fields where
      toSelect (Tuple t t2) = Select <<< SelectTuple <<< Tuple (toSelect t) $ toSelect t2
else
instance intToSelect :: ToSelect Int (SelectScalar Int) fields where
      toSelect n = Select $ SelectScalar n
--needs more instance for scalars

--for sub queries only a single column can be returned
class ToSubSelect from s fields | from -> s, s -> from where
      toSubSelect :: from -> Select s fields

instance rowToSubSelect :: Cons name t e fields => ToSubSelect (Field name) (SelectField name) fields where
      toSubSelect _ = Select SelectField
else
instance intToSubSelect :: ToSubSelect Int (SelectScalar Int) fields where
      toSubSelect n = Select $ SelectScalar n

--to catch ill typed selects soon
class IsSelectable :: forall k. k -> Constraint
class IsSelectable from

instance fieldIsSelectable :: IsSelectable (Field name)
instance intIsSelectable :: IsSelectable Int
instance tableIsSelectable :: IsSelectable (Table name fields)
instance tupleIsSelectable :: (IsSelectable from, IsSelectable from2) => IsSelectable (Tuple from from2)
instance fromIsSelectable :: IsSelectable (From f (Select s fields) fields)
instance whereIsSelectable :: IsSelectable (Where (From f (Select s fields) fields) fields parameters)

select :: forall from s fields. IsSelectable from => ToSelect from s fields => from -> Select s fields
select = toSelect



----------------------------AS----------------------------

data As fw s a (projection :: Row Type) = As fw

--as should work with where too!
class ToAs s projection | s -> projection where
      toAs :: forall name f fields. IsSymbol name => Alias name -> From f s fields -> As (From f s fields) s (Alias name) projection

instance selectScalarToAs :: ToAs (Select (SelectScalar s) fields) () where
      toAs _ s = As s

instance selectFieldToAs :: (IsSymbol name, Cons name t e fields, Cons name t () single) => ToAs (Select (SelectField name) fields) single where
      toAs _ s = As s

instance selectTupleToAs :: (ToAs s projection, ToAs s2 projection2, Union projection projection2 all) => ToAs (Select (SelectTuple (Tuple s s2)) fields) all where
      toAs _ s = As s

as :: forall f s projection name fields. IsSymbol name => ToAs s projection => Alias name -> From f s fields -> As (From f s fields) s (Alias name) projection
as a s = toAs a s



----------------------------FROM----------------------------

newtype From :: forall k. Type -> k -> Row Type -> Type
--having `s` helps with type class instances
newtype From f s (fields :: Row Type) = From f

newtype FromTable (name :: Symbol) s (fields :: Row Type) = FromTable s

data FromAs as s (fields :: Row Type) = FromAs as s

--to catch ill typed froms soon
class IsFromable :: forall k. k -> Constraint
class IsFromable from

instance fieldIsFromable :: IsFromable (Table name fields)
instance asIsFromable :: IsFromable (As fw s a projection) --ಠ_ಠ

class ToFrom from f fields | from -> f, f -> from where
      toFrom :: forall s. from -> Select s fields -> From (f (Select s fields) fields) (Select s fields) fields

instance fromTableToFrom :: ToFrom (Table name fields) (FromTable name) fields where
      toFrom _ s = From $ FromTable s
--ignore fields from Select so projection can take over
instance fromAsToFrom :: ToFrom (As f s a projection) (FromAs (As f s a projection)) projection where
      toFrom as s = From $ FromAs as s

from :: forall from f s fields. IsFromable from => ToFrom from f fields => from -> Select s fields -> From (f (Select s fields) fields) (Select s fields) fields
from f s = toFrom f s



----------------------------WHERE----------------------------

data Where f (fields :: Row Type) parameters = Where Filtered (Record parameters) f

wher :: forall f s fields parameters. Filters fields parameters -> Record parameters -> From f s fields -> Where (From f s fields) fields parameters
wher (Filters filtered) parameters before = Where filtered parameters before

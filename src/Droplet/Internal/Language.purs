-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet` instead
-- |
-- | This module defines the entire SQL EDSL, mostly because it'd be a pain to split it

module Droplet.Internal.Language where

import Droplet.Internal.Definition
import Droplet.Internal.Filter
import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Union)


--prepared is not type safe again, check invalid.purs


----------------------PREPARE----------------------------

--this shouldnt creat an actual prepare statement (since it'd require unique names and $n parameters)
-- but rather type check all parameter usage in a single place
data Prepare q (parameters :: Row Type) = Prepare q (Record parameters)

class ToPrepare q parameters | q -> parameters where
      toPrepare :: Record parameters -> q -> Prepare q parameters

instance selecToPrepare :: ToPrepare (Select f parameters fields) parameters where
      toPrepare parameters w = Prepare w parameters

instance whereToPrepare :: ToPrepare s parameters => ToPrepare (Where (From f s fields) fields has parameters) parameters where
      toPrepare parameters w = Prepare w parameters

prepare :: forall q parameters. ToPrepare q parameters => Record parameters -> q -> Prepare q parameters
prepare = toPrepare



----------------------SELECT----------------------------

newtype Select s (parameters :: Row Type) (fields :: Row Type) = Select s

class ToSelect r s parameters fields | r -> s, s -> r where
      toSelect :: r -> Select s parameters fields

--as it is, we can't express select table.* /\ table2.*
-- nor sub queries without from (which I dont know if it is ever useful)
--NEEDS TO SUPPORT PARAMETER column
instance fieldToSelect :: Cons name t e fields => ToSelect (Field name) (Field name) parameters fields where
      toSelect s = Select s

instance starToSelect :: ToSelect Star Star parameters fields where
      toSelect s = Select s

instance intToSelect :: ToSelect Int Int parameters fields where
      toSelect n = Select n
--needs more instance for scalars

instance tupleToSelect :: (ToSelect r s parameters fields, ToSelect t u parameters fields) => ToSelect (Tuple r t) (Tuple (Select s parameters fields) (Select u parameters fields)) parameters fields where
      toSelect (Tuple s t) = Select <<< Tuple (toSelect s) $ toSelect t

--an extra Select to help ToQuery instances
instance fromToSelect :: ToSubSelect s to => ToSelect (From f (Select s parameters to) to) (Select (From f (Select s parameters to) to) parameters to) parameters fields where
      toSelect fr = Select $ Select fr

instance whereToSelect :: (ToSubSelect s to) => ToSelect (Where (From f (Select s parameters to) to) to has parameters) (Select (Where (From f (Select s parameters to) to) to has parameters) parameters to) parameters fields where
      toSelect wr = Select $ Select wr

--for sub queries only a single column can be returned
class ToSubSelect r fields

instance rowToSubSelect :: Cons name t e fields => ToSubSelect (Field name) fields
instance intToSubSelect :: ToSubSelect Int fields

--to catch ill typed selects soon
-- can we get rid of this?
class IsSelectable :: forall k. k -> Constraint
class IsSelectable r

instance fieldIsSelectable :: IsSelectable (Field name)
instance intIsSelectable :: IsSelectable Int
instance tableIsSelectable :: IsSelectable Star
instance tupleIsSelectable :: (IsSelectable r, IsSelectable s) => IsSelectable (Tuple r s)
instance fromIsSelectable :: IsSelectable (From f s fields)
instance whereIsSelectable :: IsSelectable (Where f fields has parameters)

select :: forall r s parameters fields. IsSelectable r => ToSelect r s parameters fields => r -> Select s parameters fields
select = toSelect



-------------------------------FROM----------------------------

data From f s (fields :: Row Type) = From f s

class ToFrom f fields | f -> fields where
      toFrom :: forall s parameters. f -> Select s parameters fields -> From f (Select s parameters fields) fields

instance fromTableToFrom :: ToFrom (Table name fields) fields where
      toFrom table s = From table s

-- data FromAs as s (fields :: Row Type) = FromAs as s

-- instance fromAsToFrom :: ToFrom (As q a projection) (FromAs (As q a projection)) projection where
--       toFrom as s = From $ FromAs as s

-- --to catch ill typed froms soon
class IsFromable :: forall k. k -> Constraint
class IsFromable from

instance fieldIsFromable :: IsFromable (Table name fields)
-- instance asIsFromable :: IsFromable (As q a projection) --ಠ_ಠ

from :: forall f s parameters fields. IsFromable f => ToFrom f fields => f -> Select s parameters fields -> From f (Select s parameters fields) fields
from = toFrom



-------------------------------WHERE----------------------------

data Where f (fields :: Row Type) (has :: Type) parameters = Where Filtered f
--will likely change to ToWhere
-- and likely that it should have a s type parameter for selects
wher :: forall f s has fields parameters. Filters fields parameters has -> From f s fields -> Where (From f s fields) fields has parameters
wher (Filters filtered) fr = Where filtered fr



----------------------------AS----------------------------

data As q (alias :: Symbol) (projection :: Row Type) = As q

-- class ToAs q projection | q -> projection where
--       toAs :: forall name. IsSymbol name => Alias name -> q -> As q (Alias name) projection

-- --this is very tedious, aint we got a better way?
-- -- might help if previous statement was always in last position
-- instance fromSelectScalarToAs :: ToAs (From f (Select (SelectScalar s) fields) fields) () where
--       toAs _ q = As q

-- instance fromSelectFieldToAs :: (IsSymbol name, Cons name t e fields, Cons name t () single) => ToAs (From f (Select (SelectField name) fields) fields) single where
--       toAs _ q = As q

-- instance fromSelectStarToAs :: ToAs (From f (Select (SelectStar) fields) fields) fields where
--       toAs _ q = As q

-- instance fromSelectTupleToAs :: (ToAs (From f s fields) projection, ToAs (From f s2 fields) projection2, Union projection projection2 all) => ToAs (From f (Select (SelectTuple (Tuple s s2)) fields) fields) all where
--       toAs _ q = As q

-- instance whereSelectScalarToAs :: ToAs (Where (From f (Select (SelectScalar s) fields) fields) fields has parameters) () where
--       toAs _ q = As q

-- instance whereSelectFieldToAs :: (IsSymbol name, Cons name t e fields, Cons name t () single) => ToAs (Where (From f (Select (SelectField name) fields) fields) fields has parameters) single where
--       toAs _ q = As q

-- instance whereSelectStarToAs :: ToAs (Where (From f (Select (SelectStar) fields) fields) fields has parameters) fields where
--       toAs _ q = As q

-- instance whereSelectTupleToAs :: (ToAs (From f s fields) projection, ToAs (From f s2 fields) projection2, Union projection projection2 all) => ToAs (Where (From f (Select (SelectTuple (Tuple s s2)) fields) fields) fields has parametes) all where
--       toAs _ q = As q

-- as :: forall q projection name. IsSymbol name => ToAs q projection => Alias name -> q -> As q (Alias name) projection
-- as a q = toAs a q



-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet` instead
-- |
-- | This module defines the entire SQL EDSL, mostly because it'd be a pain to split it

module Droplet.Internal.Language where

import Droplet.Internal.Definition
import Droplet.Internal.Filter
import Prelude

import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Lacks, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Above, Quote, Text)


----------------------PREPARE----------------------------

--this shouldnt creat an actual prepare statement (since it'd require unique names and $n parameters)
-- but rather type check all parameter usage in a single place
data Prepare q (parameters :: Row Type) = Prepare q (Record parameters)

class ToPrepare q parameters | q -> parameters where
      toPrepare :: Record parameters -> q -> Prepare q parameters

instance selectToPrepare :: ToPrepare (Select f parameters) parameters where
      toPrepare parameters s = Prepare s parameters

instance fromAsToPrepare :: (ToPrepare q parameters, ToPrepare s parameters) => ToPrepare (From (As q a parameters projection) s parameters projection) parameters where
      toPrepare parameters w = Prepare w parameters
else
instance fromToPrepare :: ToPrepare s parameters => ToPrepare (From f s parameters fields) parameters where
      toPrepare parameters w = Prepare w parameters

instance whereToPrepare :: ToPrepare f parameters => ToPrepare (Where f has parameters) parameters where
      toPrepare parameters w = Prepare w parameters

prepare :: forall q parameters. ToPrepare q parameters => Record parameters -> q -> Prepare q parameters
prepare = toPrepare



----------------------SELECT----------------------------

newtype Select s (parameters :: Row Type) = Select s

class ToSelect r s parameters | r -> s, r -> parameters where
      toSelect :: r -> Select s parameters

--needs more instance for scalars
-- might be nice to able to project parameters too

instance fieldToSelect :: ToSelect (Field name) (Field name) parameters where
      toSelect s = Select s
else
instance starToSelect :: ToSelect Star Star parameters where
      toSelect s = Select s
else
instance intToSelect :: ToSelect Int Int parameters where
      toSelect n = Select n
else
--to desambiguate fields
instance asFieldsToSelect :: ToSubSelect q => ToSelect (As q alias parameters projection) (As q alias parameters projection) parameters where
      toSelect a = Select a
else
instance tupleToSelect :: (ToSelect r s parameters, ToSelect t u parameters) => ToSelect (Tuple r t) (Tuple (Select s parameters) (Select u parameters)) parameters where
      toSelect (Tuple s t) = Select <<< Tuple (toSelect s) $ toSelect t
else
instance fromToSelect :: Fail (Text "Since sub queries can be ambiguous, Droplet.as must be used") => ToSelect (From f s parameters to) (From f s parameters to) parameters where
      toSelect fr = Select fr
else
instance whereToSelect :: Fail (Text "Since sub queries can be ambiguous, Droplet.as must be used") => ToSelect (Where f has parameters) (Where f has parameters) parameters where
      toSelect wr = Select wr
else
--not ideal, should be able to be regular type error
instance elseToSelect :: Fail (Above (Text "Droplet.ToSelect cannot make a column out of") (Quote x)) => ToSelect x x p where
      toSelect = Select

-- we likely want to only accept if there is a limit statement
class ToSubSelect (r :: Type)

--check if these can generate invalid prepare parameter queries
--for sub queries only a single column can be returned
instance fromFieldToSubSelect :: ToSubSelect (From f (Select (Field name) parameters) parameter fields)
instance fromIntToSubSelect :: ToSubSelect (From f (Select Int parameter) parameters fields)
instance fromTupleToSubSelect :: Fail (Text "Sub queries must return a single column") => ToSubSelect (From f (Select (Tuple a b) parameter) parameters fields)
instance whereToSubSelect :: ToSubSelect f => ToSubSelect (Where f has parameters)



select :: forall r s parameters. ToSelect r s parameters => r -> Select s parameters
select = toSelect



-------------------------------FROM----------------------------

data From f s (parameters :: Row Type) (fields :: Row Type) = From f s

class ToFrom f s parameters fields | f -> s, f -> parameters, f -> fields where
      toFrom :: f -> Select s parameters -> From f (Select s parameters) parameters fields

instance tableToFrom :: (ToExtraFields s some extra, Union some extra fields) => ToFrom (Table name some) s parameters fields where
      toFrom table s = From table s
else
instance asToFrom :: (ToExtraFields s projection extra, Union projection extra fields) => ToFrom (As q a parameters projection) s parameters fields where
      toFrom as s = From as s
else
--not ideal, should be able to be regular type error
instance elseToFrom :: Fail (Above (Text "Droplet.ToFrom cannot recognize fields of") (Quote x)) => ToFrom x x p f where
      toFrom = From

--get (valid) fields referenced in the select but not present on source
class ToExtraFields (s :: Type) (source :: Row Type) (projection :: Row Type) | s -> source, s -> projection

instance intToExtraFields :: ToExtraFields Int source ()
instance fieldToExtraFields :: (Cons name t e source) => ToExtraFields (Field name) source ()
instance starToExtraFields :: ToExtraFields Star source ()
--columns from sub queries can only have a single result which needs to included aliased
instance asIntFromToExtraFields :: ToExtraFields (From f (Select Int parameters) parameters fields) () ("column" :: Int)
instance asFieldFromToExtraFields :: (Cons name t e fields, Cons name t () single) => ToExtraFields (From f (Select (Field name) parameters) parameters fields) () single
instance asWhereToExtraFields :: ToExtraFields f () extra => ToExtraFields (Where f has parameters) () extra
instance asToExtraFields :: (
      Lacks alias source,
      ToExtraFields q () extra,
      RowToList extra list,
      ToExtraType list t,
      Cons alias t () single
) => ToExtraFields (As q alias parameters projection) source single
instance tupleToExtraFields :: (ToExtraFields s source some, ToExtraFields t source more, Union some more extra) => ToExtraFields (Tuple (Select s parameters) (Select t parameters)) source extra

class ToExtraType (fields :: RowList Type) (t :: Type) | fields -> t

instance singleToExtraType :: ToExtraType (RL.Cons name t RL.Nil) t

from :: forall f s parameters fields. ToFrom f s parameters fields => f -> Select s parameters -> From f (Select s parameters) parameters fields
from = toFrom



-------------------------------WHERE----------------------------

data Where f (has :: Type) (parameters :: Row Type) = Where Filtered f

wher :: forall f s has fields parameters. Filters fields parameters has -> From f (Select s parameters) parameters fields -> Where (From f (Select s parameters) parameters fields) has parameters
wher (Filters filtered) fr = Where filtered fr



----------------------------AS----------------------------

data As q (alias :: Symbol) (parameters :: Row Type) (projection :: Row Type) = As q

--restrict it to fields that were actually selected
class ToAs q parameters projection | q -> parameters, q -> projection where
      toAs :: forall name. Alias name -> q -> As q name parameters projection

--like this, naked selects cannot aliased
instance subQueryFromToAs :: ToProjection s fields projection => ToAs (From f (Select s parameters) parameters fields) parameters projection where
      toAs _ q = As q

instance whereSelectScalarToAs :: ToAs f parameters projection => ToAs (Where f has parameters) parameters projection where
      toAs _ q = As q

class ToProjection (s :: Type) (fields :: Row Type) (projection :: Row Type) | s -> fields, s -> projection

instance intToProjection :: ToProjection Int fields ()
instance fieldToProjection :: (Cons name t e fields, Cons name t () projection) => ToProjection (Field name) fields projection
--union not needed
instance starToProjection :: Union fields () projection => ToProjection Star fields projection
--sub query as column
instance asIntFromToProjection :: ToProjection (From f (Select Int parameters) parameters fields) () ("column" :: Int)
instance asFieldFromToProjection :: (Cons name t e fields, Cons name t () single) => ToProjection (From f (Select (Field name) parameters) parameters fields) () single
instance asWhereToProjection :: ToProjection f () extra => ToProjection (Where f has parameters) () extra
instance asToProjection :: (
      ToProjection q () extra,
      RowToList extra list,
      ToExtraType list t,
      Cons alias t () single
) => ToProjection (As q alias parameters projection) fields single
instance tupleToProjection :: (ToProjection s fields some, ToProjection t fields more, Union some more extra) => ToProjection (Tuple (Select s parameters) (Select t parameters)) fields extra

as :: forall q projection parameters name. ToAs q parameters projection => Alias name -> q -> As q name parameters projection
as a q = toAs a q



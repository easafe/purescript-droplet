-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet` instead
-- |
-- | This module defines the entire SQL EDSL, mostly because it'd be a pain to split it

module Droplet.Internal.Edsl.Language where

import Droplet.Internal.Edsl.Definition
import Droplet.Internal.Edsl.Filter
import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Nub, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Above, Quote, Text)

{-

full select syntax supported by postgresql (https://www.postgresql.org/docs/current/sql-select.html)

[ WITH [ RECURSIVE ] with_query [, ...] ]
SELECT [ ALL | DISTINCT [ ON ( expression [, ...] ) ] ]
    [ * | expression [ [ AS ] output_name ] [, ...] ]
    [ FROM from_item [, ...] ]
    [ WHERE condition ]
    [ GROUP BY grouping_element [, ...] ]
    [ HAVING condition ]
    [ WINDOW window_name AS ( window_definition ) [, ...] ]
    [ { UNION | INTERSECT | EXCEPT } [ ALL | DISTINCT ] select ]
    [ ORDER BY expression [ ASC | DESC | USING operator ] [ NULLS { FIRST | LAST } ] [, ...] ]
    [ LIMIT { count | ALL } ]
    [ OFFSET start [ ROW | ROWS ] ]
    [ FETCH { FIRST | NEXT } [ count ] { ROW | ROWS } { ONLY | WITH TIES } ]
    [ FOR { UPDATE | NO KEY UPDATE | SHARE | KEY SHARE } [ OF table_name [, ...] ] [ NOWAIT | SKIP LOCKED ] [...] ]

where from_item can be one of:

    [ ONLY ] table_name [ * ] [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
                [ TABLESAMPLE sampling_method ( argument [, ...] ) [ REPEATABLE ( seed ) ] ]
    [ LATERAL ] ( select ) [ AS ] alias [ ( column_alias [, ...] ) ]
    with_query_name [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
    [ LATERAL ] function_name ( [ argument [, ...] ] )
                [ WITH ORDINALITY ] [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
    [ LATERAL ] function_name ( [ argument [, ...] ] ) [ AS ] alias ( column_definition [, ...] )
    [ LATERAL ] function_name ( [ argument [, ...] ] ) AS ( column_definition [, ...] )
    [ LATERAL ] ROWS FROM( function_name ( [ argument [, ...] ] ) [ AS ( column_definition [, ...] ) ] [, ...] )
                [ WITH ORDINALITY ] [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
    from_item [ NATURAL ] join_type from_item [ ON join_condition | USING ( join_column [, ...] ) ]

and grouping_element can be one of:

    ( )
    expression
    ( expression [, ...] )
    ROLLUP ( { expression | ( expression [, ...] ) } [, ...] )
    CUBE ( { expression | ( expression [, ...] ) } [, ...] )
    GROUPING SETS ( grouping_element [, ...] )

and with_query is:

    with_query_name [ ( column_name [, ...] ) ] AS [ [ NOT ] MATERIALIZED ] ( select | values | insert | update | delete )

TABLE [ ONLY ] table_name [ * ]

-}

{-

full select syntax supported by droplet

SELECT
      * | column | AS | SELECT | [, ...]
      [ FROM ]
      [ WHERE ]

AS
      integer | FROM output_name | WHERE output_name

FROM
      table_name | AS

WHERE
      { field | parameter } OPERATOR { field | parameter } | [ { and | or } ] | [...]

OPERATOR
      = | <> |

-}


----------------------PREPARE----------------------------

--this shouldnt create an actual prepare statement (since it'd require unique names and $n parameters)
-- but rather type check all parameter usage in a single place
-- | Unnamed statements will only employ parameter substitution
-- |
-- | Named statements will result into PREPARE
data Prepare q (parameters :: Row Type) = Prepare q (Record parameters) Plan

data Plan =
      NotNamed |
      Named String

class ToPrepare q parameters | q -> parameters where
      toPrepare :: Plan -> Record parameters -> q -> Prepare q parameters

instance selectFieldToPrepare :: ToPrepare (Select (Field name) parameters) parameters where
      toPrepare plan parameters s = Prepare s parameters plan

else instance selectAsIntToPrepare :: ToPrepare (Select (As Int a parameters projection) parameters) parameters where
      toPrepare plan parameters s = Prepare s parameters plan

else instance selectAsFieldToPrepare :: ToPrepare (Select (As (Field name) a parameters projection) parameters) parameters where
      toPrepare plan parameters s = Prepare s parameters plan

else instance selectAsToPrepare :: ToPrepare q parameters => ToPrepare (Select (As q a parameters projection) parameters) parameters where
      toPrepare plan parameters s = Prepare s parameters plan

else instance selectTupleToPrepare :: (ToPrepare s parameters, ToPrepare t parameters) => ToPrepare (Select (Tuple s t) parameters) parameters where
      toPrepare plan parameters s = Prepare s parameters plan

else instance selectElseToPrepare :: ToPrepare q parameters => ToPrepare (Select q parameters) parameters where
      toPrepare plan parameters s = Prepare s parameters plan

instance fromAsToPrepare :: (ToPrepare q parameters, ToPrepare s parameters) => ToPrepare (From (As q a parameters projection) s parameters projection) parameters where
      toPrepare plan parameters f = Prepare f parameters plan
else
instance fromToPrepare :: ToPrepare s parameters => ToPrepare (From f s parameters fields) parameters where
      toPrepare plan parameters f = Prepare f parameters plan

instance whereToPrepare :: ToPrepare f parameters => ToPrepare (Where f has parameters) parameters where
      toPrepare plan parameters w = Prepare w parameters plan

prepare :: forall q parameters. ToPrepare q parameters => Plan -> Record parameters -> q -> Prepare q parameters
prepare = toPrepare



----------------------SELECT----------------------------

newtype Select s (parameters :: Row Type) = Select s

class ToSelect r s parameters | r -> s, r -> parameters where
      toSelect :: r -> Select s parameters

--needs more instance for scalars
-- might be nice to able to project parameters too

instance fieldToSelect :: ToSelect (Field name) (Field name) parameters where
      toSelect s = Select s

else instance starToSelect :: ToSelect Star Star parameters where
      toSelect s = Select s

else instance asIntToSelect :: ToSelect (As Int alias parameters projection) (As Int alias parameters projection) parameters where
      toSelect a = Select a

else instance asFieldToSelect :: ToSelect (As (Field name) alias parameters projection) (As (Field name) alias parameters projection) parameters where
      toSelect a = Select a

else instance tupleToSelect :: (ToSelect r s parameters, ToSelect t u parameters) => ToSelect (Tuple r t) (Tuple (Select s parameters) (Select u parameters)) parameters where
      toSelect (Tuple s t) = Select <<< Tuple (toSelect s) $ toSelect t

else instance fromFieldsToSelect :: ToSubExpression q parameters => ToSelect q q parameters where
      toSelect q = Select q

class ToSubExpression (r :: Type) (parameters :: Row Type) | r -> parameters

--for sub queries only a single column can be returned
instance fromFieldToSubExpression :: ToSubExpression (From f (Select (Field name) parameters) parameters fields) parameters
instance fromIntToSubExpression :: ToSubExpression (From f (Select (As Int name parameters projection) parameters) parameters fields) parameters
instance fromAsFieldToSubExpression :: ToSubExpression (From f (Select (As (Field name) alias parameters projection) parameters) parameters fields) parameters
instance whereToSubExpression :: ToSubExpression f parameters => ToSubExpression (Where f has parameters) parameters
instance asFieldToSubExpression :: ToSubExpression q parameters => ToSubExpression (As q alias parameters projection) parameters
instance fromTupleToSubExpression :: Fail (Text "Subquery must return a single column") => ToSubExpression (From f (Select (Tuple a b) parameters) parameters fields) parameters

select :: forall r s parameters. ToSelect r s parameters => r -> Select s parameters
select = toSelect



-------------------------------FROM----------------------------

data From f s (parameters :: Row Type) (fields :: Row Type) = From f s

--we could add the projection to Select here
class ToFrom f s parameters fields | f -> s, f -> parameters, f -> fields where
      toFrom :: f -> Select s parameters -> From f (Select s parameters) parameters fields

instance tableToFrom :: (
      ToProjection (Select s parameters) fields selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (Table name fields) s parameters fields where
      toFrom table s = From table s

else instance asToFrom :: (
      ToProjection (Select s parameters) projection selected,
      Nub selected unique,
      UniqueColumnNames selected unique
) => ToFrom (As q a parameters projection) s parameters projection where
      toFrom as s = From as s

--not ideal, should be able to be regular type error
else instance elseToFrom :: Fail (Above (Text "Droplet.ToFrom cannot recognize fields of") (Quote x)) => ToFrom x x p f where
      toFrom = From

from :: forall f s parameters fields. ToFrom f s parameters fields => f -> Select s parameters -> From f (Select s parameters) parameters fields
from = toFrom



-------------------------------WHERE----------------------------

data Where f (has :: IsParameterized) (parameters :: Row Type) = Where Filtered f

wher :: forall f s has fields parameters. Filters fields parameters has -> From f (Select s parameters) parameters fields -> Where (From f (Select s parameters) parameters fields) has parameters
wher (Filters filtered) fr = Where filtered fr



----------------------------AS----------------------------

data As q (alias :: Symbol) (parameters :: Row Type) (projection :: Row Type) = As q

--restrict it to fields that were actually selected
class ToAs q name parameters projection | q -> name, q -> parameters, q -> projection where
      toAs :: Alias name -> q -> As q name parameters projection

instance intToAs :: (IsSymbol name, Cons name Int () projection) => ToAs Int name parameters projection where
      toAs _ n =  As n

instance fieldToAs :: ToAs (Field name) alias parameters () where
      toAs _ n =  As n

instance subQueryFromToAs :: ToProjection s fields projection => ToAs (From f (Select s parameters) parameters fields) name parameters projection where
      toAs _ q = As q

instance whereSelectScalarToAs :: ToAs f n parameters projection => ToAs (Where f has parameters) name parameters projection where
      toAs _ q = As q

as :: forall q projection parameters name. ToAs q name parameters projection => Alias name -> q -> As q name parameters projection
as a q = toAs a q


------------------------Projection machinery---------------------------

-- | Row Type of columns projected by the query
class ToProjection (s :: Type) (fields :: Row Type) (projection :: Row Type) | s -> fields, s -> projection

instance intAsToProjection :: ToProjection (As Int alias parameters projection) fields projection

else instance fieldAsToProjection :: (Cons name t e fields, Cons alias t e projection) => ToProjection (As (Field name) alias parameters p) fields projection

--sub query as column
--might need to fail tuples here too
else instance selectFromToProjection :: ToProjection s fields single => ToProjection (From f (Select s parameters) parameters fields) other single

else instance asWhereToProjection :: ToProjection f fields extra => ToProjection (Where f has parameters) fields extra

else instance asToProjection :: (
      ToProjection q fields extra,
      RowToList extra list,
      ToSingleColumn list t,
      Cons alias t () single
) => ToProjection (As q alias parameters projection) fields single

else instance fieldToProjection :: (Cons name t e fields, Cons name t () projection) => ToProjection (Field name) fields projection

--union not needed
else instance starToProjection :: Union fields () projection => ToProjection Star fields projection

else instance tupleToProjection :: (ToProjection s fields some, ToProjection t fields more, Union some more extra) => ToProjection (Tuple (Select s parameters) (Select t parameters)) fields extra

else instance selectToProjection :: ToProjection s fields projection => ToProjection (Select s parameters) fields projection

else instance f :: Fail (Text "Cannot recognize projection") => ToProjection x f p

class ToSingleColumn (fields :: RowList Type) (t :: Type) | fields -> t

instance singleToSingleColumn :: ToSingleColumn (RL.Cons name t RL.Nil) t

-- | Query projections should not repeat column names
class UniqueColumnNames (some :: Row Type) (more :: Row Type)

instance sameUniqueColumnNames :: UniqueColumnNames fields fields